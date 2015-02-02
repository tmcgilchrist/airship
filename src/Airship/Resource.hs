{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( Resource(..)
    , serverError
    , runResource
    , defaultResource
    ) where

import Airship.Types (Handler, finishWith, request, state)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)

import Control.Exception.Lifted (IOException, handle)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)

import Network.Wai (Response, responseLBS,
                    responseBuilder, requestMethod)
import Network.HTTP.Types (Method, methodGet, status200, status405, status500, status503)

data Resource s m =
    Resource { allowedMethods   :: Handler s m [Method]
             , serviceAvailable :: Handler s m Bool
             , content          :: Handler s m Response
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
    content

defaultResource :: Resource Integer IO
defaultResource = Resource { allowedMethods    = myAllowedMethods
                           , serviceAvailable  = myServiceAvailable
                           , content           = myContent
                           }


-- Resource examples ---------------------------------------------------------
------------------------------------------------------------------------------

myServiceAvailable :: Handler s IO Bool
myServiceAvailable = do
    liftIO $ putStrLn "During service available"
    return True

myAllowedMethods :: Handler s m [Method]
myAllowedMethods = return [methodGet]

myContent :: Show s => Handler s m Response
myContent = do
    _req <- request
    s <- state
    let myS = fromShow s
    return $ responseBuilder status200 [] myS
