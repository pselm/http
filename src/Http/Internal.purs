
module Elm.Http.Internal
    ( Request(..)
    , RawRequest
    , Expect(..)
    , Body(..)
    , Header(..)
    , Response
    )
    where

import Elm.Default

import Elm.Dict (Dict)
import Elm.Time (Time)
import Prelude (class Eq, class Functor)


newtype Request a
    = Request (RawRequest a)

derive instance functorRequest :: Functor Request


type RawRequest a =
    { method :: String
    , headers :: List Header
    , url :: String
    , body :: Body
    , expect :: Expect a
    , timeout :: Maybe Time
    , withCredentials :: Bool
    }


data Expect a
    = ExpectString (Response String -> Result String a)

derive instance functorExpect :: Functor Expect


data Body
    = EmptyBody
    | StringBody String String
    | FormDataBody

derive instance eqBody :: Eq Body


data Header
    = Header String String

derive instance eqHeader :: Eq Header


type Response body =
    { url :: String
    , status :: { code :: Int, message :: String }
    , headers :: Dict String String
    , body :: body
    }
