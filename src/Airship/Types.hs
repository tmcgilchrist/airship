{-# LANGUAGE CPP #-}
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
    , entireRequestBody
    , etagToByteString
    , eitherResponse
    , escapedResponse
    , runWebmachine
    , request
    , requestTime
    , getResponseHeaders
    , getResponseBody
    , params
    , dispatchPath
    , putResponseBody
    , putResponseBS
    , halt
    , finishWith
    , (#>)
    ) where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Writer.Class (MonadWriter, tell)
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
    Request { requestMethod      :: Method -- ^ The request method -- @GET@, @POST@, @DELETE@, et cetera.
            , httpVersion        :: HttpVersion -- ^ The HTTP version (usually 1.1; hopefully someday 2.0).
            , rawPathInfo        :: ByteString -- ^ The unparsed path information yielded from the WAI server. You probably want 'pathInfo'.
            , rawQueryString     :: ByteString -- ^ The query string, if any, yielded from the WAI server. You probably want 'queryString'.
            , requestHeaders     :: RequestHeaders -- ^ An association list of (headername, value) pairs. See "Network.HTTP.Types.Header" for the possible values.
            , isSecure           :: Bool -- ^ Was this request made over SSL/TLS?
            , remoteHost         :: SockAddr -- ^ The address information of the client.
            , pathInfo           :: [Text] -- ^ The URL, stripped of hostname and port, split on forward-slashes
            , queryString        :: Query -- ^ Parsed query string information.
            , requestBody        :: m ByteString -- ^ A monadic action that extracts a (possibly-empty) chunk of the request body.
            , requestBodyLength  :: Wai.RequestBodyLength -- ^ Either @ChunkedBody@ or a @KnownLength 'Word64'@.
            , requestHeaderHost  :: Maybe ByteString -- ^ Contains the Host header.
            , requestHeaderRange :: Maybe ByteString -- ^ Contains the Range header.
            , waiRequest         :: Wai.Request
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
    , waiRequest = Wai.defaultRequest
    }

-- | Reads the entirety of the request body in a single string.
-- This turns the chunks obtained from repeated invocations of 'requestBody' into a lazy 'ByteString'.
entireRequestBody :: Monad m => Request m -> m LB.ByteString
entireRequestBody req = requestBody req >>= strictRequestBody' LB.empty
    where strictRequestBody' acc prev
            | BS.null prev = return acc
            | otherwise = requestBody req >>= strictRequestBody' (acc <> LB.fromStrict prev)

data RequestReader m = RequestReader { _now :: UTCTime
                                     , _request :: Request m
                                     }

data ETag = Strong ByteString
          | Weak ByteString
          deriving (Eq, Ord)

instance Show ETag where show = unpack . etagToByteString

etagToByteString :: ETag -> ByteString
etagToByteString (Strong bs) = "\"" <> bs <> "\""
etagToByteString (Weak bs) = "W/\"" <> bs <> "\""

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

data ResponseState m = ResponseState { stateHeaders   :: ResponseHeaders
                                     , stateBody      :: ResponseBody m
                                     , _params        :: HashMap Text Text
                                     , _dispatchPath  :: [Text]
                                     }

type Trace = [Text]

newtype Webmachine m a =
    Webmachine { getWebmachine :: EitherT (Response m) (RWST (RequestReader m) Trace (ResponseState m) m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader (RequestReader m),
                  MonadWriter Trace,
                  MonadState (ResponseState m))

instance MonadTrans Webmachine where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

newtype StMWebmachine m a = StMWebmachine {
      unStMWebmachine :: StM (EitherT (Response m) (RWST (RequestReader m) Trace (ResponseState m) m)) a
    }

instance MonadBaseControl b m => MonadBaseControl b (Webmachine m) where
  type StM (Webmachine m) a = StMWebmachine m a
  liftBaseWith f = Webmachine
                     $ liftBaseWith
                     $ \g' -> f
                     $ \m -> liftM StMWebmachine
                     $ g' $ getWebmachine m
  restoreM = Webmachine . restoreM . unStMWebmachine

-- | A convenience synonym that writes the @Monad@ type constraint for you.
type Handler m a = Monad m => Webmachine m a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

-- | Returns the 'Request' that this 'Handler' is currently processing.
request :: Handler m (Request m)
request = _request <$> ask

-- | Returns the bound routing parameters extracted from the routing system (see "Airship.Route").
params :: Handler m (HashMap Text Text)
params = _params <$> get

dispatchPath :: Handler m [Text]
dispatchPath = _dispatchPath <$> get

-- | Returns the time at which this request began processing.
requestTime :: Handler m UTCTime
requestTime = _now <$> ask

-- | Returns the 'ResponseHeaders' stored in the current 'Handler'.
getResponseHeaders :: Handler m ResponseHeaders
getResponseHeaders = stateHeaders <$> get

-- | Returns the current 'ResponseBody' that this 'Handler' is storing.
getResponseBody :: Handler m (ResponseBody m)
getResponseBody = stateBody <$> get

-- | Given a new 'ResponseBody', replaces the stored body with the new one.
putResponseBody :: ResponseBody m -> Handler m ()
putResponseBody b = modify updateState
    where updateState rs = rs {stateBody = b}

-- | Stores the provided 'ByteString' as the responseBody. This is a shortcut for
-- creating a response body with a 'ResponseBuilder' and a bytestring 'Builder'.
putResponseBS :: ByteString -> Handler m ()
putResponseBS bs = putResponseBody $ ResponseBuilder $ fromByteString bs

-- | Immediately halts processing with the provided 'Status' code.
-- The contents of the 'Handler''s response body will be streamed back to the client.
-- This is a shortcut for constructing a 'Response' with 'getResponseHeaders' and 'getResponseBody'
-- and passing that response to 'finishWith'.
halt :: Status -> Handler m a
halt status = finishWith =<< Response <$> pure status <*> getResponseHeaders <*> getResponseBody

-- | Immediately halts processing and writes the provided 'Response' back to the client.
finishWith :: Response m -> Handler m a
finishWith = Webmachine . left

-- | The @#>@ operator provides syntactic sugar for the construction of association lists.
-- For example, the following assoc list:
--
-- @
--     [("run", "jewels"), ("blue", "suede"), ("zion", "wolf")]
-- @
--
-- can be represented as such:
--
-- @
--     execWriter $ do
--       "run" #> "jewels"
--       "blue" #> "suede"
--       "zion" #> "wolf"
-- @
--
-- It used in 'RoutingSpec' declarations to indicate that a particular 'Route' maps
-- to a given 'Resource', but can be used in many other places where association lists
-- are expected, such as 'contentTypesProvided'.
(#>) :: MonadWriter [(k, v)] m => k -> v -> m ()
k #> v = tell [(k, v)]

both :: Either a a -> a
both = either id id

eitherResponse :: Monad m => UTCTime -> HashMap Text Text -> [Text] -> Request m -> Handler m (Response m) -> m (Response m, Trace)
eitherResponse reqDate reqParams dispatched req resource = do
    (e, trace) <- runWebmachine reqDate reqParams dispatched req resource
    return (both e, trace)

runWebmachine :: Monad m => UTCTime -> HashMap Text Text -> [Text] -> Request m -> Handler m a -> m (Either (Response m) a, Trace)
runWebmachine reqDate reqParams dispatched req w = do
    let startingState = ResponseState [] Empty reqParams dispatched
        requestReader = RequestReader reqDate req
    (e, _, t) <- runRWST (runEitherT (getWebmachine w)) requestReader startingState
    return (e, t)
