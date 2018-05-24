
-- | > Create and send HTTP requests.
-- |
-- | This is implemented using the low-level bindings in purescript-web-xhr.
-- | An alternative approach to HTTP requests in Purescript would use
-- | [purescript-affjax](https://pursuit.purescript.org/packages/purescript-affjax/5.0.0),
-- | but its API doesn't quite match up for implementing this module.

module Elm.Http
    ( Request, send, Error(..)
    , getString, get
    , post
    , request
    , Header, header
    , Body, emptyBody, jsonBody, stringBody, multipartBody, Part, stringPart
    , Expect, expectString, expectJson, expectStringResponse, Response
    , encodeUri, decodeUri, toTask
    )
    where

import Elm.Default

import Control.Monad.Aff (Error) as Aff
import Control.Monad.Aff (effCanceler)
import Control.Monad.Eff (Eff)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.XHR.FormData (FormDataValue(..), toFormData)
import DOM.XHR.Types (FormData) as DOM.XHR.Types
import Data.Either (Either(..))
import Data.Foldable (class Foldable, for_)
import Data.List (List(..))
import Elm.Http.Internal (Body(..), Expect(..), Header(..), Request(..), Response) as Http.Internal
import Elm.Http.Internal (RawRequest)
import Elm.Json.Decode as Decode
import Elm.Json.Encode as Encode
import Elm.Maybe (Maybe(..))
import Elm.Result (Result(..))
import Elm.Task (Task, makeTask)
import Elm.Task (attempt) as Task
import Elm.Time (Time, fromTime)
import Prelude (class Eq, bind, discard, pure, ($), (>>>))
import Unsafe.Coerce (unsafeCoerce)
import Web.XHR (FormData, send) as Web.XHR
import Web.XHR (XMLHttpRequest, abort, open, sendFormData, sendString, setRequestHeader, setTimeout, setWithCredentials, string, xmlHttpRequest, xmlHttpRequestToEventTarget)


-- REQUESTS


-- | > Describes an HTTP request.
type Request a =
    Http.Internal.Request a


-- | > Send a `Request`. We could get the text of “War and Peace” like this:
-- | >
-- | >     import Http
-- | >
-- | >     type Msg = Click | NewBook (Result Http.Error String)
-- | >
-- | >     update : Msg -> Model -> Model
-- | >     update msg model =
-- | >       case msg of
-- | >         Click ->
-- | >           ( model, getWarAndPeace )
-- | >
-- | >         NewBook (Ok book) ->
-- | >           ...
-- | >
-- | >         NewBook (Err _) ->
-- | >           ...
-- | >
-- | >     getWarAndPeace : Cmd Msg
-- | >     getWarAndPeace =
-- | >       Http.send NewBook <|
-- | >         Http.getString "https://example.com/books/war-and-peace.md"
send :: ∀ msg a. (Result Error a -> msg) -> Request a -> Cmd msg
send resultToMessage req =
    Task.attempt resultToMessage (toTask req)


-- | > Convert a `Request` into a `Task`. This is only really useful if you want
-- | > to chain together a bunch of requests (or any other tasks) in a single command.
toTask :: ∀ a. Request a -> Task Error a
toTask (Http.Internal.Request req) =
    -- TODO: Add the progress checking parts
    makeTask \cb -> do
        -- For now, it's always a string response type, but that could change
        -- in future.
        xmlReq <- xmlHttpRequest string

        let target = xmlHttpRequestToEventTarget xmlReq
        let errorHandler = eventListener \_ -> cb $ Right $ Left $ NetworkError
        let timeoutHandler = eventListener \_ -> cb $ Right $ Left $ Timeout

        addEventListener (EventType "error") errorHandler false target
        addEventListener (EventType "timeout") timeoutHandler false target
        addEventListener (EventType "load") (handleResponse xmlReq req cb) false target

        -- TODO: The Elm code catches exceptions from `open` and constructs
        -- a `BadUrl` error.
        open req.method req.url xmlReq

        for_ req.headers \(Http.Internal.Header key value) ->
            setRequestHeader key value xmlReq

        for_ req.timeout \t ->
            setTimeout (fromTime t) xmlReq

        setWithCredentials req.withCredentials xmlReq

        case req.body of
            Http.Internal.EmptyBody ->
                Web.XHR.send xmlReq

            Http.Internal.FormDataBody formData ->
                sendFormData (coerceFormData formData) xmlReq

            Http.Internal.StringBody contentType payload -> do
                setRequestHeader "Content-Type" contentType xmlReq
                sendString payload xmlReq

        pure $ effCanceler $ abort xmlReq


-- | These are both representations of the Javascript FormData, but
-- | they aren't integrated ...
coerceFormData :: DOM.XHR.Types.FormData -> Web.XHR.FormData
coerceFormData = unsafeCoerce


handleResponse :: ∀ e a x y. XMLHttpRequest x -> RawRequest a -> (Either Aff.Error (Either Error a) -> Eff e y) -> EventListener e
handleResponse xmlReq req cb =
    eventListener \_ ->
        cb $ Right $ Left $ Timeout


-- | > A `Request` can fail in a couple ways:
-- | >
-- | >   - `BadUrl` means you did not provide a valid URL.
-- | >   - `Timeout` means it took too long to get a response.
-- | >   - `NetworkError` means the user turned off their wifi, went in a cave, etc.
-- | >   - `BadStatus` means you got a response back, but the [status code][sc]
-- | >     indicates failure.
-- | >   - `BadPayload` means you got a response back with a nice status code, but
-- | >     the body of the response was something unexpected. The `String` in this
-- | >     case is a debugging message that explains what went wrong with your JSON
-- | >     decoder or whatever.
-- | >
-- | > [sc]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
data Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus (Response String)
    | BadPayload String (Response String)

derive instance eqError :: Eq Error


-- GET


-- | > Create a `GET` request and interpret the response body as a `String`.
-- | >
-- | >     import Http
-- | >
-- | >     getWarAndPeace : Http.Request String
-- | >     getWarAndPeace =
-- | >       Http.getString "https://example.com/books/war-and-peace"
getString :: String -> Request String
getString url =
    request
        { method : "GET"
        , headers : Nil
        , url : url
        , body : emptyBody
        , expect : expectString
        , timeout : Nothing
        , withCredentials : false
        }


-- | > Create a `GET` request and try to decode the response body from JSON to
-- | > some Elm value.
-- | >
-- | >     import Http
-- | >     import Json.Decode exposing (list, string)
-- | >
-- | >     getBooks : Http.Request (List String)
-- | >     getBooks =
-- | >       Http.get "https://example.com/books" (list string)
-- | >
-- | > You can learn more about how JSON decoders work [here][] in the guide.
-- | >
-- | > [here]: https://guide.elm-lang.org/interop/json.html
get :: ∀ a. String -> Decode.Decoder a -> Request a
get url decoder =
    request
        { method : "GET"
        , headers : Nil
        , url : url
        , body : emptyBody
        , expect : expectJson decoder
        , timeout : Nothing
        , withCredentials : false
        }



-- POST


-- | > Create a `POST` request and try to decode the response body from JSON to
-- | > an Elm value. For example, if we want to send a POST without any data in the
-- | > request body, it would be like this:
-- | >
-- | >     import Http
-- | >     import Json.Decode exposing (list, string)
-- | >
-- | >     postBooks : Http.Request (List String)
-- | >     postBooks =
-- | >       Http.post "https://example.com/books" Http.emptyBody (list string)
-- | >
-- | > See [`jsonBody`](#jsonBody) to learn how to have a more interesting request
-- | > body. And check out [this section][here] of the guide to learn more about
-- | > JSON decoders.
-- | >
-- | > [here]: https://guide.elm-lang.org/interop/json.html
post :: ∀ a. String -> Body -> Decode.Decoder a -> Request a
post url body decoder =
    request
        { method : "POST"
        , headers : Nil
        , url : url
        , body : body
        , expect : expectJson decoder
        , timeout : Nothing
        , withCredentials : false
        }



-- CUSTOM REQUESTS


-- | > Create a custom request. For example, a custom PUT request would look like
-- | > this:
-- | >
-- | >     put : String -> Body -> Request ()
-- | >     put url body =
-- | >       request
-- | >         { method = "PUT"
-- | >         , headers = []
-- | >         , url = url
-- | >         , body = body
-- | >         , expect = expectStringResponse (\_ -> Ok ())
-- | >         , timeout = Nothing
-- | >         , withCredentials = False
-- | >         }
request :: ∀ a.
    { method :: String
    , headers :: List Header
    , url :: String
    , body :: Body
    , expect :: Expect a
    , timeout :: Maybe Time
    , withCredentials :: Bool
    }
    -> Request a
request =
    Http.Internal.Request



-- HEADERS


-- | > An HTTP header for configuring requests. See a bunch of common headers
-- | > [here][].
-- | >
-- | > [here]: https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
type Header =
    Http.Internal.Header


-- | > Create a `Header`.
-- | >
-- | >     header "If-Modified-Since" "Sat 29 Oct 1994 19:43:31 GMT"
-- | >     header "Max-Forwards" "10"
-- | >     header "X-Requested-With" "XMLHttpRequest"
-- | >
-- | > **Note:** In the future, we may split this out into an `Http.Headers` module
-- | > and provide helpers for cases that are common on the client-side. If this
-- | > sounds nice to you, open an issue [here][] describing the helper you want and
-- | > why you need it.
-- | >
-- | > [here]: https://github.com/elm-lang/http/issues
header :: String -> String -> Header
header =
    Http.Internal.Header



-- BODY


-- | > Represents the body of a `Request`.
type Body =
    Http.Internal.Body


-- | > Create an empty body for your `Request`. This is useful for GET requests
-- | > and POST requests where you are not sending any data.
emptyBody :: Body
emptyBody =
    Http.Internal.EmptyBody


-- | > Put some JSON value in the body of your `Request`. This will automatically
-- | > add the `Content-Type: application/json` header.
jsonBody :: Encode.Value -> Body
jsonBody value =
    Http.Internal.StringBody "application/json" (Encode.encode 0 value)


-- | > Put some string in the body of your `Request`. Defining `jsonBody` looks
-- | > like this:
-- | >
-- | >     import Json.Encode as Encode
-- | >
-- | >     jsonBody : Encode.Value -> Body
-- | >     jsonBody value =
-- | >       stringBody "application/json" (Encode.encode 0 value)
-- | >
-- | > Notice that the first argument is a [MIME type][mime] so we know to add
-- | > `Content-Type: application/json` to our request headers. Make sure your
-- | > MIME type matches your data. Some servers are strict about this!
-- | >
-- | > [mime]: https://en.wikipedia.org/wiki/Media_type
stringBody :: String -> String -> Body
stringBody =
    Http.Internal.StringBody


-- | > Create multi-part bodies for your `Request`, automatically adding the
-- | > `Content-Type: multipart/form-data` header.
multipartBody :: ∀ f. Foldable f => f Part -> Body
multipartBody = toFormData >>> Http.Internal.FormDataBody


-- | > Contents of a multi-part body. Right now it only supports strings, but we
-- | > will support blobs and files when we get an API for them in Elm.
type Part = String /\ FormDataValue


-- | > A named chunk of string data.
-- | >
-- | >     body =
-- | >       multipartBody
-- | >         [ stringPart "user" "tom"
-- | >         , stringPart "payload" "42"
-- | >         ]
stringPart :: String -> String -> Part
stringPart key value =
    Tuple key (FormDataString value)



-- RESPONSES


-- | > Logic for interpreting a response body.
type Expect a =
    Http.Internal.Expect a


-- | > Expect the response body to be a `String`.
expectString :: Expect String
expectString =
    expectStringResponse (\response -> Ok response.body)


-- | > Expect the response body to be JSON. You provide a `Decoder` to turn that
-- | > JSON into an Elm value. If the body cannot be parsed as JSON or if the JSON
-- | > does not match the decoder, the request will resolve to a `BadPayload` error.
expectJson :: ∀ a. Decode.Decoder a -> Expect a
expectJson decoder =
    expectStringResponse (\response -> Decode.decodeString decoder response.body)


-- | > Maybe you want the whole `Response`: status code, headers, body, etc. This
-- | > lets you get all of that information. From there you can use functions like
-- | > `Json.Decode.decodeString` to interpret it as JSON or whatever else you want.
expectStringResponse :: ∀ a. (Response String -> Result String a) -> Expect a
expectStringResponse = Http.Internal.ExpectString


-- | > The response from a `Request`.
type Response body =
    Http.Internal.Response body



-- LOW-LEVEL

-- | > Use this to escape query parameters. Converts characters like `/` to `%2F`
-- | > so that it does not clash with normal URL
-- | >
-- | > It work just like `encodeURIComponent` in JavaScript.
foreign import encodeUri :: String -> String


-- | > Use this to unescape query parameters. It converts things like `%2F` to
-- | > `/`. It can fail in some cases. For example, there is no way to unescape `%`
-- | > because it could never appear alone in a properly escaped string.
-- | >
-- | > It works just like `decodeURIComponent` in JavaScript.
decodeUri :: String -> Maybe String
decodeUri = decodeUriImpl Just Nothing


foreign import decodeUriImpl :: (∀ a. a -> Maybe a) -> (∀ a. Maybe a) -> String -> Maybe String
