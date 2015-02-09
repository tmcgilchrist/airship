{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( Resource(..)
    , serverError
    , runResource
    , defaultResource
    , singletonContentType
    ) where

import Airship.Types (Webmachine, Handler, Response(..), ResponseBody(..),
                      finishWith, request)

import Control.Monad (unless, when)

import Data.HashMap.Strict (HashMap, singleton)
import Data.Text (Text)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import Network.Wai (requestMethod)
import Network.HTTP.Types

data Resource s m =
    Resource { allowedMethods           :: Handler s m [Method]
             , serviceAvailable         :: Handler s m Bool
             , implemented              :: Handler s m Bool
             , resourceExists           :: Handler s m Bool
             , isAuthorized             :: Handler s m Bool
             , forbidden                :: Handler s m Bool
             , allowMissingPost         :: Handler s m Bool
             , malformedRequest         :: Handler s m Bool
             , uriTooLong               :: Handler s m Bool
             , knownContentType         :: Handler s m Bool
             , validContentHeaders      :: Handler s m Bool
             , entityTooLarge           :: Handler s m Bool
             , deleteResource           :: Handler s m Bool
             , deleteCompleted          :: Handler s m Bool
             , postIsCreate             :: Handler s m Bool
             , createPath               :: Handler s m (Maybe Text)
             , processPost              :: Handler s m Bool
             -- can't for the life of me figure out why the inner 'Webmachine'
             -- can't be a 'Handler', this it is probably something to do with
             -- our (maybe?) inappropriate use of type aliases for constraints?
             , contentTypesProvided     :: Handler s m (HashMap Text (Webmachine s m (Response m)))
             }


serverError :: Handler m s a
serverError = finishWith (Response status500 [] Empty)

runResource :: Resource s IO -> Handler s IO (Response IO)
runResource Resource{..} = do
    req <- request

    let finish s r = finishWith (Response s [] (ResponseBuilder (fromByteString r)))

    available <- serviceAvailable
    unless available $
      finish status503 "503 Service Unavailable"

    impl <- implemented
    unless impl $
      finish status501 "501 Not Implemented"

    tooLong <- uriTooLong
    when tooLong $
      finish status414 "414 Request URI Too Long"

    acceptableMethods <- allowedMethods
    unless (requestMethod req `elem` acceptableMethods) $
      finish status405 "405 Method Not Allowed"

    malformed <- malformedRequest
    when malformed $
      finish status400 "400 Bad Request"

    authorized <- isAuthorized
    unless authorized $ 
      finish status401 "401 Unauthorized"

    can't <- forbidden
    when can't $ 
      finish status403 "403 Forbidden"

    acceptableContent <- validContentHeaders
    unless acceptableContent $
      finish status501 "501 Not Implemented"

    acceptableType <- knownContentType
    unless acceptableType $
      finish status415 "415 Unsupported Media Type"

    tooLarge <- entityTooLarge
    when tooLarge $
      finish status413 "413 Request Entity Too Large"

    finish status200 "200 OK"
    

defaultResource :: Resource s m
defaultResource = Resource { allowedMethods         = return [methodGet]
                           , serviceAvailable       = return True
                           , implemented            = return True
                           , isAuthorized           = return True
                           , resourceExists         = return True
                           , forbidden              = return False
                           , allowMissingPost       = return False
                           , malformedRequest       = return False
                           , uriTooLong             = return False
                           , knownContentType       = return True
                           , validContentHeaders    = return True
                           , entityTooLarge         = return False
                           , deleteResource         = return False
                           , deleteCompleted        = return False
                           , postIsCreate           = return False
                           , createPath             = return Nothing
                           , processPost            = return False
                           -- should this be a map?
                           , contentTypesProvided   = return (singleton "text/html" helloWorld)
                           }

helloWorld :: Handler s m (Response m)
helloWorld = return (Response status200 [] (ResponseBuilder (fromByteString "Hello, world")))

singletonContentType :: Text -> Text -> Handler s m (HashMap Text (Webmachine s m (Response m)))
singletonContentType ct tex = return (singleton ct (return (Response status200 [] (ResponseBuilder (fromHtmlEscapedText tex)))))
