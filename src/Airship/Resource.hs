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

import Airship.Types (Webmachine, Handler, finishWith, request)

import Control.Exception.Lifted (IOException, handle)
import Control.Monad (unless)

import Data.HashMap.Strict (HashMap, singleton)
import Data.Text (Text)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import Network.Wai (Response, responseLBS, responseBuilder, requestMethod)
import Network.HTTP.Types (Method, methodGet, status200, status405, status500, status503)

data Resource s m =
    Resource { allowedMethods           :: Handler s m [Method]
             , serviceAvailable         :: Handler s m Bool
             , resourceExists           :: Handler s m Bool
             , isAuthorized             :: Handler s m Bool
             , forbidden                :: Handler s m Bool
             , allowMissingPost         :: Handler s m Bool
             , malformedRequest         :: Handler s m Bool
             , uriTooLong               :: Handler s m Bool
             , knownContentType         :: Handler s m Bool
             , validContentHeaders      :: Handler s m Bool
             , validEntityLength        :: Handler s m Bool
             , deleteResource           :: Handler s m Bool
             , deleteCompleted          :: Handler s m Bool
             , postIsCreate             :: Handler s m Bool
             , createPath               :: Handler s m (Maybe Text)
             , processPost              :: Handler s m Bool
             -- can't for the life of me figure out why the inner 'Webmachine'
             -- can't be a 'Handler', this it is probably something to do with
             -- our (maybe?) inappropriate use of type aliases for constraints?
             , contentTypesProvided     :: Handler s m (HashMap Text (Webmachine s m Response))
             }


serverError :: Handler m s a
serverError = finishWith (responseLBS status500 [] "")

runResource :: Resource s IO -> Handler s IO Response
runResource Resource{..} = do
    req <- request

    let catcher (_e :: IOException) = serverError
    handle catcher $ do
        -- bail out earlier if the request method is incorrect
        acceptableMethods <- allowedMethods
        unless (requestMethod req `elem` acceptableMethods) $ finishWith (responseLBS status405 [] "")

    -- bail out early if the service is not available
    available <- serviceAvailable
    unless available $ finishWith (responseLBS status503 [] "")

    -- otherwise return the normal response
    undefined

defaultResource :: Resource s m
defaultResource = Resource { allowedMethods         = return [methodGet]
                           , serviceAvailable       = return True
                           , isAuthorized           = return True
                           , resourceExists         = return True
                           , forbidden              = return False
                           , allowMissingPost       = return False
                           , malformedRequest       = return False
                           , uriTooLong             = return False
                           , knownContentType       = return True
                           , validContentHeaders    = return True
                           , validEntityLength      = return True
                           , deleteResource         = return False
                           , deleteCompleted        = return False
                           , postIsCreate           = return False
                           , createPath             = return Nothing
                           , processPost            = return False
                           -- should this be a map?
                           , contentTypesProvided   = return (singleton "text/html" helloWorld)
                           }

helloWorld :: Handler s m Response
helloWorld = return (responseLBS status200 [] "Hello, world")

singletonContentType :: Text -> Text -> Handler s m (HashMap Text (Webmachine s m Response))
singletonContentType ct tex = return (singleton ct (return (responseBuilder status200 [] (fromHtmlEscapedText tex))))
