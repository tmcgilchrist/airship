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
    , responseHeaders
    , responseBody
    , halt
    , finishWith
    ) where

import Blaze.ByteString.Builder (Builder)

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..),
                                    MonadTransControl(..), defaultLiftBaseWith,
                                    defaultRestoreM)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Writer.Class (MonadWriter)

import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))

import Network.HTTP.Types (ResponseHeaders, Status)

import qualified Network.Wai as Wai

data ResponseState s = ResponseState { userState  :: s
                                     , headers    :: ResponseHeaders
                                     , body       :: Maybe Builder
                                     }

data Trace = Trace deriving (Show)

instance Monoid Trace where
    mempty      = Trace
    mappend _ _ = Trace

newtype Webmachine s m a =
    Webmachine { getWebmachine :: EitherT Wai.Response (RWST Wai.Request Trace (ResponseState s) m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader Wai.Request,
                  MonadWriter Trace,
                  MonadState (ResponseState s))

instance MonadTrans (Webmachine s) where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

instance MonadTransControl (Webmachine s) where
    type StT (Webmachine s) a = StT (RWST Wai.Request Trace (ResponseState s)) (StT (EitherT Wai.Response) a)
    liftWith f = Webmachine . liftWith $ \r -> liftWith $ \r' -> f $ r' . r . getWebmachine
    restoreT = Webmachine . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (Webmachine s m) where
  type StM (Webmachine s m) a = ComposeSt (Webmachine s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM  = defaultRestoreM

type Handler s m a = Monad m => Webmachine s m a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

request :: Handler m s Wai.Request
request = ask

state :: Handler s m s
state = userState <$> get

putState :: s -> Handler s m ()
putState s = modify updateState
    where updateState rs = rs {userState = s}

modifyState :: (s -> s) -> Handler s m ()
modifyState f = modify modifyState'
    where modifyState' rs@ResponseState{userState=uState} =
                                        rs {userState = f uState}

responseHeaders :: Handler m s ResponseHeaders
responseHeaders = headers <$> get

responseBody :: Handler m s (Maybe Builder)
responseBody = body <$> get

halt :: Status -> Handler m s a
halt status = do
    respHeaders <- responseHeaders
    mBuilder <- responseBody
    let builder = fromMaybe mempty mBuilder
        response = Wai.responseBuilder status respHeaders builder
    finishWith response

finishWith :: Wai.Response -> Handler m s a
finishWith = Webmachine . left

both :: Either a a -> a
both = either id id

eitherResponse :: Monad m => Wai.Request -> s -> Handler s m Wai.Response -> m Wai.Response
eitherResponse req s resource = do
    e <- runWebmachine req s resource
    return $ both e

runWebmachine :: Monad m => Wai.Request -> s -> Handler s m a -> m (Either Wai.Response a)
runWebmachine req s w = do
    let startingState = ResponseState s [] Nothing
    (e, _, _) <- runRWST (runEitherT (getWebmachine w)) req startingState
    return e
