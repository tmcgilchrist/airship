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
    , Request(..)
    , Response(..)
    , ResponseState(..)
    , ResponseBody(..)
    , defaultRequest
    , strictRequestBody
    , eitherResponse
    , escapedResponse
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
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Control.Applicative
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

import Network.Socket (SockAddr(..))
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types ( ResponseHeaders
                          , RequestHeaders
                          , Query
                          , Status
                          , Method
                          , HttpVersion )

import qualified Network.Wai as Wai

-- | Very similar to WAI's @Request@ type, except generalized to an arbitrary monad @m@.
data Request m =
    Request { requestMethod :: Method -- ^ The request method -- @GET@, @POST@, @DELETE@, et cetera.
            , httpVersion :: HttpVersion -- ^ The HTTP version (usually 1.1; hopefully someday 2.0).
            , rawPathInfo :: ByteString -- ^ The unparsed path information yielded from the WAI server. You probably want 'pathInfo'.
            , rawQueryString :: ByteString -- ^ The query string, if any, yielded from the WAI server. You probably want 'queryString'.
            , requestHeaders :: RequestHeaders -- ^ An association list of (headername, value) pairs. See "Network.HTTP.Types.Header" for the possible values.
            , isSecure :: Bool -- ^ Was this request made over SSL/TLS?
            , remoteHost :: SockAddr -- ^ The address information of the client.
            , pathInfo :: [Text] -- ^ The URL, stripped of hostname and port, split on forward-slashes
            , queryString :: Query -- ^ Parsed query string information.
            , requestBody :: m ByteString -- ^ A monadic action that extracts a (possibly-empty) chunk of the request body.
            , requestBodyLength :: Wai.RequestBodyLength -- ^ Either @ChunkedBody@ or a @KnownLength 'Word64'@.
            , requestHeaderHost :: Maybe ByteString -- ^ Contains the Host header.
            , requestHeaderRange :: Maybe ByteString -- ^ Contains the Range header.
            }

defaultRequest :: Monad m => Request m
defaultRequest = Request
    { requestMethod = HTTP.methodGet
    , httpVersion = HTTP.http10
    , rawPathInfo = BS.empty
    , rawQueryString = BS.empty
    , requestHeaders = []
    , isSecure = False
    , remoteHost = SockAddrInet 0 0
    , pathInfo = []
    , queryString = []
    , requestBody = return BS.empty
    , requestBodyLength = Wai.KnownLength 0
    , requestHeaderHost = Nothing
    , requestHeaderRange = Nothing
    }

-- | Extracts the entirety of a request body from a given 'Request', taking into account chunked requests.
-- Despite the name, this function actually returns a lazy 'ByteString'.
strictRequestBody :: Monad m => Request m -> m LB.ByteString
strictRequestBody req = requestBody req >>= strictRequestBody' LB.empty
    where strictRequestBody' acc prev
            | BS.null prev = return acc
            | otherwise = requestBody req >>= strictRequestBody' (acc <> LB.fromStrict prev)

data RequestReader m = RequestReader { _now :: UTCTime
                                     , _request :: Request m
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

-- | Helper function for building a `ResponseBuilder` out of HTML-escaped text.
escapedResponse :: Text -> ResponseBody m
escapedResponse = ResponseBuilder . fromHtmlEscapedText

data Response m = Response { _responseStatus     :: Status
                           , _responseHeaders    :: ResponseHeaders
                           , _responseBody       :: ResponseBody m
                           }

data ResponseState s m = ResponseState { stateUser      :: s
                                       , stateHeaders   :: ResponseHeaders
                                       , stateBody      :: ResponseBody m
                                       , _params :: HashMap Text Text
                                       }

type Trace = [Text]

newtype Webmachine s m a =
    Webmachine { getWebmachine :: EitherT (Response m) (RWST (RequestReader m) Trace (ResponseState s m) m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader (RequestReader m),
                  MonadWriter Trace,
                  MonadState (ResponseState s m))

instance MonadTrans (Webmachine s) where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

newtype StMWebmachine s m a = StMWebmachine {
      unStMWebmachine :: StM (EitherT (Response m) (RWST (RequestReader m) Trace (ResponseState s m) m)) a
    }

instance MonadBaseControl b m => MonadBaseControl b (Webmachine s m) where
  type StM (Webmachine s m) a = StMWebmachine s m a
  liftBaseWith f = Webmachine
                     $ liftBaseWith
                     $ \g' -> f
                     $ \m -> liftM StMWebmachine
                     $ g' $ getWebmachine m
  restoreM = Webmachine . restoreM . unStMWebmachine

-- | A convenience synonym that writes the @Monad@ type constraint for you.
type Handler s m a = Monad m => Webmachine s m a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

-- | Returns the 'Request' that this 'Handler' is currently processing.
request :: Handler s m (Request m)
request = _request <$> ask

-- | Returns the bound routing parameters extracted from the routing system (see "Airship.Route").
params :: Handler s m (HashMap Text Text)
params = _params <$> get

-- | Returns the time at which this request began processing.
requestTime :: Handler s m UTCTime
requestTime = _now <$> ask

-- | Returns the user state (of type @s@) in the provided @'Handler' s m@.
getState :: Handler s m s
getState = stateUser <$> get

-- | Sets the user state.
putState :: s -> Handler s m ()
putState s = modify updateState
    where updateState rs = rs {stateUser = s}

-- | Applies the provided function to the user state.
modifyState :: (s -> s) -> Handler s m ()
modifyState f = modify modifyState'
    where modifyState' rs@ResponseState{stateUser=uState} =
                                        rs {stateUser = f uState}

-- | Returns the 'ResponseHeaders' stored in the current 'Handler'.
getResponseHeaders :: Handler s m ResponseHeaders
getResponseHeaders = stateHeaders <$> get

-- | Returns the current 'ResponseBody' that this 'Handler' is storing.
getResponseBody :: Handler s m (ResponseBody m)
getResponseBody = stateBody <$> get

-- | Given a new 'ResponseBody', replaces the stored body with the new one.
putResponseBody :: ResponseBody m -> Handler s m ()
putResponseBody b = modify updateState
    where updateState rs = rs {stateBody = b}

-- | Stores the provided 'ByteString' as the responseBody. This is a shortcut for
-- creating a response body with a 'ResponseBuilder' and a bytestring 'Builder'.
putResponseBS :: ByteString -> Handler s m ()
putResponseBS bs = putResponseBody $ ResponseBuilder $ fromByteString bs

-- | Immediately halts processing with the provided 'Status' code.
-- The contents of the 'Handler''s response body will be streamed back to the client.
-- This is a shortcut for constructing a 'Response' with 'getResponseHeaders' and 'getResponseBody'
-- and passing that response to 'finishWith'.
halt :: Status -> Handler m s a
halt status = finishWith =<< Response <$> pure status <*> getResponseHeaders <*> getResponseBody

-- | Immediately halts processing and writes the provided 'Response' back to the client.
finishWith :: Response m -> Handler s m a
finishWith = Webmachine . left

both :: Either a a -> a
both = either id id

eitherResponse :: Monad m => UTCTime -> HashMap Text Text -> Request m -> s -> Handler s m (Response m) -> m (Response m, Trace)
eitherResponse reqDate reqParams req s resource = do
    (e, trace) <- runWebmachine reqDate reqParams req s resource
    return (both e, trace)

runWebmachine :: Monad m => UTCTime -> HashMap Text Text -> Request m -> s -> Handler s m a -> m (Either (Response m) a, Trace)
runWebmachine reqDate reqParams req s w = do
    let startingState = ResponseState s [] Empty reqParams
        requestReader = RequestReader reqDate req
    (e, _, t) <- runRWST (runEitherT (getWebmachine w)) requestReader startingState
    return (e, t)
