{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( PostResponse(..)
    , Resource(..)
    , serverError
    , defaultResource
    , singletonContentType
    ) where

import Airship.Types (ContentType, Handler, Response(..), ResponseBody(..),
                      finishWith)

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import Network.HTTP.Types

-- Credit for this idea goes to Richard Wallace (purefn) on Webcrank.
-- This creates an enumeration for the four possibilities of two binary
-- decisions:
-- postIsCreate = True | False
-- redirect     = True | False
data PostResponse s m
    = PostCreate [Text] -- ^ Treat this request as a PUT
    | PostCreateRedirect [Text] -- ^ Treat this request as a PUT, then redirect
    | PostProcess (Handler s m ()) -- ^ Process, but don't redirect
    | PostProcessRedirect (Handler s m ByteString)

data Resource s m =
    Resource { allowMissingPost         :: Handler s m Bool
             , allowedMethods           :: Handler s m [Method]
             , contentTypesProvided     :: Handler s m [(ContentType, ResponseBody m)]
             , createPath               :: Handler s m (Maybe Text)
             , deleteCompleted          :: Handler s m Bool
             , deleteResource           :: Handler s m Bool
             , entityTooLarge           :: Handler s m Bool
             , forbidden                :: Handler s m Bool
             , implemented              :: Handler s m Bool
             , isAuthorized             :: Handler s m Bool
             , isConflict               :: Handler s m Bool
             , knownContentType         :: Handler s m Bool
             , lastModified             :: Handler s m (Maybe UTCTime)
             , languageAvailable        :: Handler s m Bool
             , malformedRequest         :: Handler s m Bool
                                        -- wondering if this should be text,
                                        -- or some 'path' type
             , movedPermanently         :: Handler s m (Maybe ByteString)
             , movedTemporarily         :: Handler s m (Maybe ByteString)
             , multipleChoices          :: Handler s m Bool
             , previouslyExisted        :: Handler s m Bool
             , processPost              :: Handler s m (PostResponse s m)
             , resourceExists           :: Handler s m Bool
             , serviceAvailable         :: Handler s m Bool
             , uriTooLong               :: Handler s m Bool
             , validContentHeaders      :: Handler s m Bool
             }


serverError :: Handler m s a
serverError = finishWith (Response status500 [] Empty)

defaultResource :: Resource s m
defaultResource = Resource { allowMissingPost       = return False
                           , allowedMethods         = return [methodGet, methodHead]
                           , contentTypesProvided   = return [("text/html", helloWorld)]
                           , createPath             = return Nothing
                           , deleteCompleted        = return False
                           , deleteResource         = return False
                           , entityTooLarge         = return False
                           , forbidden              = return False
                           , implemented            = return True
                           , isAuthorized           = return True
                           , isConflict             = return False
                           , knownContentType       = return True
                           , lastModified           = return Nothing
                           , languageAvailable      = return True
                           , malformedRequest       = return False
                           , movedPermanently       = return Nothing
                           , movedTemporarily       = return Nothing
                           , multipleChoices        = return False
                           , previouslyExisted      = return False
                           , processPost            = return (PostProcess (return ()))
                           , resourceExists         = return True
                           , serviceAvailable       = return True
                           , uriTooLong             = return False
                           , validContentHeaders    = return True
                           }

helloWorld :: ResponseBody m
helloWorld = ResponseBuilder (fromByteString "Hello, world!")

singletonContentType :: ContentType -> Text -> [(ContentType, ResponseBody m)]
singletonContentType ct tex = [(ct, ResponseBuilder (fromHtmlEscapedText tex))]
