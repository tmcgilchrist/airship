{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
    ( Webmachine(..)
    , Resource(..)
    , request
    , state
    , putState
    , modifyState
    , finishWith
    , main
    , runWebmachine
    , resourceToWai
    ) where

import Blaze.ByteString.Builder.Char.Utf8 (fromShow)

import Control.Applicative (Applicative)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)

import Network.Wai (Application, Request, Response, responseLBS, responseBuilder, requestMethod)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (Method, methodGet, status200, status405, status503)

newtype Webmachine m s a =
    Webmachine { getWebmachine :: EitherT Response (RWST Request [Integer] s m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadReader Request,
                  MonadWriter [Integer], MonadState s)

type Handler m s a = Monad m => Webmachine m s a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

request :: Handler m s Request
request = ask

state :: Handler m s s
state = get

putState :: s -> Handler m s ()
putState = put

modifyState :: (s -> s) -> Handler m s ()
modifyState = modify

finishWith :: Response -> Handler m s a
finishWith res = Webmachine (left res)

------------------------------------------------------------------------------
------------------------------------------------------------------------------

runWebmachine :: Monad m => Request -> s -> Handler m s a -> m (Either Response a)
runWebmachine req s w = do
    (e, _, _) <- runRWST (runEitherT (getWebmachine w)) req s
    return e

data Resource m s =
    Resource { allowedMethods   :: Handler m s [Method]
             , serviceAvailable :: Handler m s Bool
             , content          :: Handler m s Response
             }

eitherResponse :: Monad m => Request -> s -> Handler m s Response -> m Response
eitherResponse req s resource = do
    e <- runWebmachine req s resource
    case e of
        (Left r) -> return r
        (Right r) -> return r

runResource :: Resource m s -> Handler m s Response
runResource Resource{..} = do
    req <- request

    -- bail out earlier if the request method is incorrect
    acceptableMethods <- allowedMethods
    unless (requestMethod req `elem` acceptableMethods) $ finishWith (responseLBS status405 [] "")

    -- bail out early if the service is not available
    available <- serviceAvailable
    unless available $ finishWith (responseLBS status503 [] "")

    -- otherwise return the normal response
    content

resourceToWai :: Resource IO s -> s -> Application
resourceToWai resource s req respond = do
    response <- eitherResponse req s (runResource resource)
    respond response

-- Resource examples ---------------------------------------------------------
------------------------------------------------------------------------------

myServiceAvailable :: Handler IO s Bool
myServiceAvailable = do
    liftIO $ putStrLn "During service available"
    return True

myAllowedMethods :: Handler m s [Method]
myAllowedMethods = return [methodGet]

myContent :: Show s => Handler m s Response
myContent = do
    _req <- request
    s <- get
    let myS = fromShow s
    return $ responseBuilder status200 [] myS

main :: IO ()
main = do
    let port = 3000
        s = 5 :: Integer
        resource = Resource { allowedMethods    = myAllowedMethods
                            , serviceAvailable  = myServiceAvailable
                            , content           = myContent }

    run port (resourceToWai resource s)
