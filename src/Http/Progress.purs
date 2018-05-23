
-- | > Track the progress of an HTTP request. This can be useful if you are
-- | > requesting a large amount of data and want to show the user a progress bar
-- | > or something.
-- | >
-- | > Here is an example usage: [demo][] and [code][].
-- | >
-- | > [demo]: https://hirafuji.com.br/elm/http-progress-example/
-- | > [code]: https://gist.github.com/pablohirafuji/fa373d07c42016756d5bca28962008c4
-- | >
-- | > **Note:** If you stop tracking progress, you cancel the request.

module Elm.Http.Progress
    ( Progress(..)
    , track
    )
    where

import Elm.Default
import Elm.Process

import Data.List (List(..))
import Elm.Dict as Dict
import Elm.Foldable (foldl) as Elm.Foldlable
import Elm.Http as Http
import Elm.Http.Internal (Request(Request))
import Elm.Http.Internal as Http.Internal
import Elm.Platform (Router) as Platform
import Elm.Platform (Router, Manager, subscription)
import Elm.Process (Id, kill, spawn) as Process
import Elm.Task (Task)
import Elm.Task (andThen, sequence, succeed) as Task
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Functor, Unit, map)
import Type.Proxy (Proxy)


-- PROGRESS


-- | > The progress of an HTTP request.
-- | >
-- | > You start with `None`. As data starts to come in, you will see `Some`. The
-- | > `bytesExpected` field will match the `Content-Length` header, indicating how
-- | > long the response body is in bytes (8-bits). The `bytes` field indicates how
-- | > many bytes have been loaded so far, so if you want progress as a percentage,
-- | > you would say:
-- | >
-- | >     Some { bytes, bytesExpected } ->
-- | >       toFloat bytes / toFloat bytesExpected
-- | >
-- | > You will end up with `Fail` or `Done` depending on the success of the request.
data Progress d
    = None
    | Some { bytes :: Int, bytesExpected :: Int }
    | Fail Http.Error
    | Done d



-- TRACK


-- | > Create a subscription that tracks the progress of an HTTP request.
-- | >
-- | > See it in action in this example: [demo][] and [code][].
-- | >
-- | > [demo]: https://hirafuji.com.br/elm/http-progress-example/
-- | > [code]: https://gist.github.com/pablohirafuji/fa373d07c42016756d5bca28962008c4
track :: ∀ msg a. String -> (Progress a -> msg) -> Http.Request a -> Sub msg
track id toMessage request =
    subscription httpProgressManager <| Track id tracked
    
    where
        tracked :: _
        tracked =
            TrackedRequest
                { request : map (Done >> toMessage) request
                , toProgress : Some >> toMessage
                , toError : Fail >> toMessage
                }


newtype TrackedRequest msg = TrackedRequest
    { request :: Http.Internal.Request msg
    , toProgress :: { bytes :: Int, bytesExpected :: Int } -> msg
    , toError :: Http.Error -> msg
    }

derive instance functorTrackedRequest :: Functor TrackedRequest



-- SUBSCRIPTIONS


data MySub msg
    = Track String (TrackedRequest msg)

derive instance functorMySub :: Functor MySub


-- EFFECT MANAGER

httpProgressManager :: Manager Proxy MySub Unit State
httpProgressManager = {init, onEffects, onSelfMsg, tag}


tag :: String
tag = "Elm.Http.Progress"


type State msg =
    Dict.Dict String Process.Id


init :: ∀ msg. Task Never (State msg)
init =
    Task.succeed Dict.empty



-- APP MESSAGES


onEffects :: ∀ msg. Platform.Router msg Unit -> List (Proxy msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs state =
    let
        subDict =
            collectSubs subs

        leftStep id process ( dead /\ ongoing /\ new ) =
            (Process.kill process : dead)
            /\ ongoing
            /\ new

        bothStep id process _ ( dead /\ ongoing /\ new ) =
            ( dead
            /\ Dict.insert id process ongoing
            /\ new
            )

        rightStep id trackedRequest ( dead /\ ongoing /\ new ) =
            ( dead
            /\ ongoing
            /\ ( id /\ trackedRequest ) : new
            )

        ( dead /\ ongoing /\ new ) =
            Dict.merge leftStep bothStep rightStep state subDict ( Nil /\ Dict.empty /\ Nil )
    in
        Task.sequence dead
            |> Task.andThen (\_ -> spawnRequests router new ongoing)


spawnRequests :: ∀ msg. Router msg Unit -> List ( String /\ TrackedRequest msg ) -> State msg -> Task Never (State msg)
spawnRequests router trackedRequests state =
    case trackedRequests of
        Nil ->
            Task.succeed state

        ( id /\ trackedRequest ) : others ->
            Process.spawn (toTask router trackedRequest)
                |> Task.andThen (\process -> spawnRequests router others (Dict.insert id process state))


toTask :: ∀ msg. Router msg Unit -> TrackedRequest msg -> Task Never Unit
toTask router (TrackedRequest { request, toProgress, toError }) =
    unsafeCrashWith "TODO"
    {-
    Native.Http.toTask request (Just (Platform.sendToApp router << toProgress))
        |> Task.andThen (Platform.sendToApp router)
        |> Task.onError (Platform.sendToApp router << toError)
    -}



-- COLLECT SUBS AS DICT


type SubDict msg =
    Dict.Dict String (TrackedRequest msg)


collectSubs :: ∀ msg. List (MySub msg) -> SubDict msg
collectSubs subs =
    Elm.Foldlable.foldl addSub Dict.empty subs


addSub :: ∀ msg. MySub msg -> SubDict msg -> SubDict msg
addSub (Track id tracked@(TrackedRequest trackedRequest)) subDict =
    let
        (Request request) =
            trackedRequest.request

        uid =
            id ++ request.method ++ request.url
    in
        Dict.insert uid tracked subDict



-- SELF MESSAGES


onSelfMsg :: ∀ msg. Platform.Router msg Unit -> Unit -> State msg -> Task Never (State msg)
onSelfMsg router _ state =
    Task.succeed state
