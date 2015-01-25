{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Control.Exception (Exception, IOException, catch)
import Control.Monad (unless)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), control, defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Writer.Class (MonadWriter)

import Network.Wai (Application, Request, Response, responseLBS, responseBuilder, requestMethod)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (Method, methodGet, status200, status405, status500, status503)

-- Monad Control fun
------------------------------------------------------------------------------

liftedCatch :: (MonadBaseControl IO m, Exception e) => m a -> (e -> m a) -> m a
liftedCatch f h = control $ \runInIO -> catch (runInIO f) (runInIO . h)

newtype Webmachine s m a =
    Webmachine { getWebmachine :: EitherT Response (RWST Request [Integer] s m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader Request,
                  MonadWriter [Integer],
                  MonadState s)

instance MonadTrans (Webmachine s) where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

instance MonadTransControl (Webmachine s) where
    type StT (Webmachine s) a = StT (RWST Request [Integer] s) (StT (EitherT Response) a)
    liftWith f = Webmachine . liftWith $ \r -> liftWith $ \r' -> f $ r' . r . getWebmachine
    restoreT = Webmachine . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (Webmachine s m) where
  type StM (Webmachine s m) a = ComposeSt (Webmachine s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM  = defaultRestoreM

-- TODO fix the order of args to handler
type Handler s m a = Monad m => Webmachine s m a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

request :: Handler m s Request
request = ask

state :: Handler s m s
state = get

putState :: s -> Handler s m ()
putState = put

modifyState :: (s -> s) -> Handler s m ()
modifyState = modify

finishWith :: Response -> Handler m s a
finishWith res = Webmachine (left res)

serverError :: Handler m s a
serverError = finishWith (responseLBS status500 [] "")

------------------------------------------------------------------------------
------------------------------------------------------------------------------

runWebmachine :: Monad m => Request -> s -> Handler s m a -> m (Either Response a)
runWebmachine req s w = do
    (e, _, _) <- runRWST (runEitherT (getWebmachine w)) req s
    return e

data Resource s m=
    Resource { allowedMethods   :: Handler s m [Method]
             , serviceAvailable :: Handler s m Bool
             , content          :: Handler s m Response
             }

-- | Grab whichever value 'a' is in Either
both :: Either a a -> a
both e = case e of
    (Left a)    -> a
    (Right a)   -> a

eitherResponse :: Monad m => Request -> s -> Handler s m Response -> m Response
eitherResponse req s resource = do
    e <- runWebmachine req s resource
    return $ both e

runResource :: Resource s IO -> Handler s IO Response
runResource Resource{..} = do
    req <- request

    let catcher (_e :: IOException) = serverError
    flip liftedCatch catcher $ do
        -- bail out earlier if the request method is incorrect
        acceptableMethods <- allowedMethods
        unless (requestMethod req `elem` acceptableMethods) $ finishWith (responseLBS status405 [] "")

    -- bail out early if the service is not available
    available <- serviceAvailable
    unless available $ finishWith (responseLBS status503 [] "")

    -- otherwise return the normal response
    content

resourceToWai :: Resource s IO -> s -> Application
resourceToWai resource s req respond = do
    response <- eitherResponse req s (runResource resource)
    respond response

-- Resource examples ---------------------------------------------------------
------------------------------------------------------------------------------

myServiceAvailable :: Handler s IO Bool
myServiceAvailable = do
    liftIO $ putStrLn "During service available"
    return True

myAllowedMethods :: Handler m s [Method]
myAllowedMethods = return [methodGet]

myContent :: Show s => Handler s m Response
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
