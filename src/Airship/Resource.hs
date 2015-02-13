{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( Resource(..)
    , ContentType
    , serverError
    , defaultResource
    , singletonContentType
    ) where

import Airship.Types (ContentType, Handler, Response(..), ResponseBody(..),
                      finishWith)

import Data.Text (Text)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import Network.HTTP.Date (HTTPDate)
import Network.HTTP.Types

data Resource s m =
    Resource { allowMissingPost         :: Handler s m Bool
             , allowedMethods           :: Handler s m [Method]
             -- can't for the life of me figure out why the inner 'Webmachine'
             -- can't be a 'Handler', this it is probably something to do with
             -- our (maybe?) inappropriate use of type aliases for constraints?
             , contentTypesProvided     :: Handler s m [(ContentType, ResponseBody m)]
             , createPath               :: Handler s m (Maybe Text)
             , deleteCompleted          :: Handler s m Bool
             , deleteResource           :: Handler s m Bool
             , entityTooLarge           :: Handler s m Bool
             , forbidden                :: Handler s m Bool
             , implemented              :: Handler s m Bool
             , isAuthorized             :: Handler s m Bool
             , knownContentType         :: Handler s m Bool
             , lastModified             :: Handler s m (Maybe HTTPDate)
             , languageAvailable        :: Handler s m Bool
             , malformedRequest         :: Handler s m Bool
             , postIsCreate             :: Handler s m Bool
             , processPost              :: Handler s m Bool
             , resourceExists           :: Handler s m Bool
             , serviceAvailable         :: Handler s m Bool
             , uriTooLong               :: Handler s m Bool
             , validContentHeaders      :: Handler s m Bool
             }


serverError :: Handler m s a
serverError = finishWith (Response status500 [] Empty)

defaultResource :: Resource s m
defaultResource = Resource { allowMissingPost       = return False
                           , allowedMethods         = return [methodGet]
                           , contentTypesProvided   = return [("text/html", helloWorld)]
                           , createPath             = return Nothing
                           , deleteCompleted        = return False
                           , deleteResource         = return False
                           , entityTooLarge         = return False
                           , forbidden              = return False
                           , implemented            = return True
                           , isAuthorized           = return True
                           , knownContentType       = return True
                           , lastModified           = return Nothing
                           , languageAvailable      = return True
                           , malformedRequest       = return False
                           , postIsCreate           = return False
                           , processPost            = return False
                           , resourceExists         = return True
                           , serviceAvailable       = return True
                           , uriTooLong             = return False
                           , validContentHeaders    = return True
                           }

helloWorld :: ResponseBody m
helloWorld = ResponseBuilder (fromByteString "Hello, world!")

singletonContentType :: ContentType -> Text -> [(ContentType, ResponseBody m)]
singletonContentType ct tex = [(ct, ResponseBuilder (fromHtmlEscapedText tex))]
