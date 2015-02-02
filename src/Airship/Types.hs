{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Airship.Types
    ( Webmachine
    , Handler
    , eitherResponse
    , runWebmachine
    , request
    , state
    , putState
    , modifyState
    , finishWith
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..),
                                    MonadTransControl(..), defaultLiftBaseWith,
                                    defaultRestoreM)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Writer.Class (MonadWriter)

import Network.Wai (Request, Response)

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
finishWith = Webmachine . left

both :: Either a a -> a
both = either id id

eitherResponse :: Monad m => Request -> s -> Handler s m Response -> m Response
eitherResponse req s resource = do
    e <- runWebmachine req s resource
    return $ both e

runWebmachine :: Monad m => Request -> s -> Handler s m a -> m (Either Response a)
runWebmachine req s w = do
    (e, _, _) <- runRWST (runEitherT (getWebmachine w)) req s
    return e
