module Examples.Cats where

import Elm.Default

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.IO (INFINITY, runIO)
import Elm.Html (Html, br, button, div, h2, img, program, text)
import Elm.Html.Attributes (src)
import Elm.Html.Events (onClick)
import Elm.Http as Http
import Elm.Json.Decode as Decode
import Elm.Platform (runProgram)
import Prelude (class Eq, Unit, unit, ($))


main :: Eff (infinity :: INFINITY) Unit
main =
    launchAff_ $ runIO do
        runProgram unit $
            program
                { init : init "cats"
                , view
                , update
                , subscriptions
                }


-- MODEL


type Model =
  { topic :: String
  , gifUrl :: String
  }


init :: String -> (Model /\ Cmd Msg)
init topic =
  { topic
  , gifUrl : "waiting.gif"
  }
  /\ getRandomGif topic



-- UPDATE


data Msg
  = MorePlease
  | NewGif (Result Http.Error String)

derive instance eqMsg :: Eq Msg


update :: Msg -> Model -> (Model /\ Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model /\ getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      { topic : model.topic
      , gifUrl : newUrl
      }
      /\ none

    NewGif (Err _) ->
      (model /\ none)



-- VIEW


view :: Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    ]



-- SUBSCRIPTIONS


subscriptions :: Model -> Sub Msg
subscriptions model =
  none



-- HTTP


getRandomGif :: String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl :: Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
