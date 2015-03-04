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
    ( ETag(..)
    , Webmachine
    , Handler
    , Response(..)
    , ResponseState(..)
    , ResponseBody(..)
    , eitherResponse
    , runWebmachine
    , request
    , requestTime
    , getState
    , putState
    , modifyState
    , getResponseHeaders
    , getResponseBody
    , params
    , putResponseBody
    , putResponseBS
    , halt
    , finishWith
    ) where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Applicative (Applicative, (<$>))
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Writer.Class (MonadWriter)
import Data.ByteString.Char8
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Network.HTTP.Types (ResponseHeaders, Status)

import qualified Network.Wai as Wai

data RequestReader = RequestReader { _now :: UTCTime
                                   , _request :: Wai.Request
                                   , _params :: HashMap Text Text
                                   }

data ETag = Strong ByteString
          | Weak ByteString
          deriving (Eq)

instance Show ETag where
    show (Strong bs)    =         "\""  <> unpack bs <> "\""
    show (Weak bs)      = "W/" <> "\""  <> unpack bs <> "\""

type StreamingBody m = (Builder -> m ()) -> m () -> m ()

-- | Basically Wai's unexported 'Response' type, but generalized to any monad,
-- 'm'.
data ResponseBody m
    = ResponseFile FilePath (Maybe Wai.FilePart)
    | ResponseBuilder Builder
    | ResponseStream (StreamingBody m)
    | Empty
    -- ResponseRaw ... (not implemented yet, but useful for websocket upgrades)

data Response m = Response { _responseStatus     :: Status
                           , _responseHeaders    :: ResponseHeaders
                           , _responseBody       :: ResponseBody m
                           }

data ResponseState s m = ResponseState { stateUser      :: s
                                       , stateHeaders   :: ResponseHeaders
                                       , stateBody      :: ResponseBody m
                                       }

type Trace = [Text]

newtype Webmachine s m a =
    Webmachine { getWebmachine :: EitherT (Response m) (RWST RequestReader Trace (ResponseState s m) m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader RequestReader,
                  MonadWriter Trace,
                  MonadState (ResponseState s m))

instance MonadTrans (Webmachine s) where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

newtype StMWebmachine s m a = StMWebmachine {
      unStMWebmachine :: StM (EitherT (Response m) (RWST RequestReader Trace (ResponseState s m) m)) a
    }

instance MonadBaseControl b m => MonadBaseControl b (Webmachine s m) where
  type StM (Webmachine s m) a = StMWebmachine s m a
  liftBaseWith f = Webmachine
                     $ liftBaseWith
                     $ \g' -> f
                     $ \m -> liftM StMWebmachine
                     $ g' $ getWebmachine m
  restoreM = Webmachine . restoreM . unStMWebmachine

type Handler s m a = Monad m => Webmachine s m a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

request :: Handler m s Wai.Request
request = _request <$> ask

params :: Handler m s (HashMap Text Text)
params = _params <$> ask

requestTime :: Handler m s UTCTime
requestTime = _now <$> ask

getState :: Handler s m s
getState = stateUser <$> get

putState :: s -> Handler s m ()
putState s = modify updateState
    where updateState rs = rs {stateUser = s}

modifyState :: (s -> s) -> Handler s m ()
modifyState f = modify modifyState'
    where modifyState' rs@ResponseState{stateUser=uState} =
                                        rs {stateUser = f uState}

getResponseHeaders :: Handler s m ResponseHeaders
getResponseHeaders = stateHeaders <$> get

getResponseBody :: Handler s m (ResponseBody m)
getResponseBody = stateBody <$> get

putResponseBody :: ResponseBody m -> Handler s m ()
putResponseBody b = modify updateState
    where updateState rs = rs {stateBody = b}

putResponseBS :: ByteString -> Handler s m ()
putResponseBS bs = putResponseBody $ ResponseBuilder $ fromByteString bs

halt :: Status -> Handler m s a
halt status = do
    respHeaders <- getResponseHeaders
    body <- getResponseBody
    let response = Response status respHeaders body
    finishWith response

finishWith :: Response m -> Handler s m a
finishWith = Webmachine . left

both :: Either a a -> a
both = either id id

eitherResponse :: Monad m => UTCTime -> HashMap Text Text -> Wai.Request -> s -> Handler s m (Response m) -> m (Response m, Trace)
eitherResponse reqDate reqParams req s resource = do
    (e, trace) <- runWebmachine reqDate reqParams req s resource
    return (both e, trace)

runWebmachine :: Monad m => UTCTime -> HashMap Text Text -> Wai.Request -> s -> Handler s m a -> m (Either (Response m) a, Trace)
runWebmachine reqDate reqParams req s w = do
    let startingState = ResponseState s [] Empty
        requestReader = RequestReader reqDate req reqParams
    (e, _, t) <- runRWST (runEitherT (getWebmachine w)) requestReader startingState
    return (e, t)
