{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( Resource(..)
    , serverError
    , defaultResource
    , singletonContentType
    ) where

import Airship.Types (Handler, Response(..), ResponseBody(..), finishWith)

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import Network.HTTP.Types

type ContentType = CI ByteString

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
             , contentTypesProvided     :: Handler s m [(ContentType, ResponseBody m)]
             }


serverError :: Handler m s a
serverError = finishWith (Response status500 [] Empty)

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
                           , contentTypesProvided   = return [("text/html", helloWorld)]
                           }

helloWorld :: ResponseBody m
helloWorld = ResponseBuilder (fromByteString "Hello, world!")

singletonContentType :: ContentType -> Text -> [(ContentType, ResponseBody m)]
singletonContentType ct tex = [(ct, ResponseBuilder (fromHtmlEscapedText tex))]
