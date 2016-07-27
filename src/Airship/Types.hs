{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Airship.Types
    ( ETag(..)
    , Webmachine
    , Request(..)
    , Response(..)
    , ResponseState(..)
    , ResponseBody(..)
    , ErrorResponses
    , addTrace
    , defaultRequest
    , entireRequestBody
    , etagToByteString
    , eitherResponse
    , escapedResponse
    , mapWebmachine
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
    , decisionTrace
    , finishWith
    , (#>)
    ) where

import           Airship.RST
import           Blaze.ByteString.Builder            (Builder)
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Blaze.ByteString.Builder.Html.Utf8  (fromHtmlEscapedText)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LB
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad                       (liftM)
import           Control.Monad.Base                  (MonadBase)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Morph
import           Control.Monad.Reader.Class          (MonadReader, ask)
import           Control.Monad.State.Class           (MonadState, get, modify,
                                                      modify')
import           Control.Monad.Trans.Control         (MonadBaseControl (..))
import           Control.Monad.Trans.Either          (EitherT (..), left,
                                                      mapEitherT, runEitherT)
import           Control.Monad.Writer.Class          (MonadWriter, tell)
import           Data.ByteString.Char8               hiding (reverse)
import           Data.HashMap.Strict                 (HashMap)
import           Data.Map.Strict                     (Map)
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Data.Time.Clock                     (UTCTime)
import           Network.HTTP.Media
import qualified Network.HTTP.Types                  as HTTP

import           Network.HTTP.Types                  (ResponseHeaders, Status)

import           Network.Wai                         (Request (..),
                                                      defaultRequest)
import qualified Network.Wai                         as Wai


-- | Reads the entirety of the request body in a single string.
-- This turns the chunks obtained from repeated invocations of 'requestBody' into a lazy 'ByteString'.
entireRequestBody :: MonadIO m => Request -> m LB.ByteString
entireRequestBody req = liftIO (requestBody req) >>= strictRequestBody' LB.empty
    where strictRequestBody' acc prev
            | BS.null prev = return acc
            | otherwise = liftIO (requestBody req) >>= strictRequestBody' (acc <> LB.fromStrict prev)

data RequestReader = RequestReader { _now     :: UTCTime
                                   , _request :: Request
                                   }

data ETag = Strong ByteString
          | Weak ByteString
          deriving (Eq, Ord)

instance Show ETag where show = unpack . etagToByteString

etagToByteString :: ETag -> ByteString
etagToByteString (Strong bs) = "\"" <> bs <> "\""
etagToByteString (Weak bs) = "W/\"" <> bs <> "\""

-- | Basically Wai's unexported 'Response' type.
data ResponseBody
    = ResponseFile FilePath (Maybe Wai.FilePart)
    | ResponseBuilder Builder
    | ResponseStream Wai.StreamingBody
    | Empty
    -- ResponseRaw ... (not implemented yet, but useful for websocket upgrades)

-- | Helper function for building a `ResponseBuilder` out of HTML-escaped text.
escapedResponse :: Text -> ResponseBody
escapedResponse = ResponseBuilder . fromHtmlEscapedText

data Response = Response { _responseStatus  :: Status
                         , _responseHeaders :: ResponseHeaders
                         , _responseBody    :: ResponseBody
                         }

data ResponseState = ResponseState { stateHeaders  :: ResponseHeaders
                                   , stateBody     :: ResponseBody
                                   , _params       :: HashMap Text Text
                                   , _dispatchPath :: [Text]
                                   , decisionTrace :: Trace
                                   }

type Trace = [ByteString]

type ErrorResponses m = Monad m => Map HTTP.Status [(MediaType, Webmachine m ResponseBody)]

newtype Webmachine m a =
    Webmachine { getWebmachine :: EitherT Response (RST RequestReader ResponseState m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader RequestReader,
                  MonadState ResponseState)

instance MonadTrans Webmachine where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

newtype StMWebmachine m a = StMWebmachine {
      unStMWebmachine :: StM (EitherT Response (RST RequestReader ResponseState m)) a
    }

instance MonadBaseControl b m => MonadBaseControl b (Webmachine m) where
  type StM (Webmachine m) a = StMWebmachine m a
  liftBaseWith f = Webmachine
                     $ liftBaseWith
                     $ \g' -> f
                     $ \m -> liftM StMWebmachine
                     $ g' $ getWebmachine m
  restoreM = Webmachine . restoreM . unStMWebmachine

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

-- | Returns the 'Request' that is currently being processed.
request :: Monad m => Webmachine m Request
request = _request <$> ask

-- | Returns the bound routing parameters extracted from the routing system (see "Airship.Route").
params :: Monad m => Webmachine m (HashMap Text Text)
params = _params <$> get

dispatchPath :: Monad m => Webmachine m [Text]
dispatchPath = _dispatchPath <$> get

-- | Returns the time at which this request began processing.
requestTime :: Monad m => Webmachine m UTCTime
requestTime = _now <$> ask

-- | Returns the current 'ResponseHeaders'.
getResponseHeaders :: Monad m => Webmachine m ResponseHeaders
getResponseHeaders = stateHeaders <$> get

-- | Returns the current 'ResponseBody'.
getResponseBody :: Monad m => Webmachine m ResponseBody
getResponseBody = stateBody <$> get

-- | Given a new 'ResponseBody', replaces the stored body with the new one.
putResponseBody :: Monad m => ResponseBody -> Webmachine m ()
putResponseBody b = modify' updateState
    where updateState rs = rs {stateBody = b}

-- | Stores the provided 'ByteString' as the responseBody. This is a shortcut for
-- creating a response body with a 'ResponseBuilder' and a bytestring 'Builder'.
putResponseBS :: Monad m => ByteString -> Webmachine m ()
putResponseBS bs = putResponseBody $ ResponseBuilder $ fromByteString bs

-- | Immediately halts processing with the provided 'Status' code.
-- The contents of the 'Webmachine''s response body will be streamed back to the client.
-- This is a shortcut for constructing a 'Response' with 'getResponseHeaders' and 'getResponseBody'
-- and passing that response to 'finishWith'.
halt :: Monad m => Status -> Webmachine m a
halt status = finishWith =<< Response <$> return status <*> getResponseHeaders <*> getResponseBody

-- | Immediately halts processing and writes the provided 'Response' back to the client.
finishWith :: Monad m => Response -> Webmachine m a
finishWith = Webmachine . left

-- | Adds the provided ByteString to the Airship-Trace header.
addTrace :: Monad m => ByteString -> Webmachine m ()
addTrace t = modify' (\s -> s { decisionTrace = t : decisionTrace s })

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

eitherResponse :: Monad m => UTCTime -> HashMap Text Text -> [Text] -> Request -> Webmachine m Response -> m (Response, Trace)
eitherResponse reqDate reqParams dispatched req resource = do
    (e, trace) <- runWebmachine reqDate reqParams dispatched req resource
    return (both e, trace)

-- | Map both the return value and wrapped computation @m@.
mapWebmachine :: ( m1 (Either Response a1, ResponseState)
                -> m2 (Either Response a2, ResponseState))
              -> Webmachine m1 a1 -> Webmachine m2 a2
mapWebmachine f =  Webmachine . (mapEitherT $ mapRST f) . getWebmachine

runWebmachine :: Monad m => UTCTime -> HashMap Text Text -> [Text] -> Request -> Webmachine m a -> m (Either (Response) a, Trace)
runWebmachine reqDate reqParams dispatched req w = do
    let startingState = ResponseState [] Empty reqParams dispatched []
        requestReader = RequestReader reqDate req
    (e, s) <- runRST (runEitherT (getWebmachine w)) requestReader startingState
    return (e, reverse $ decisionTrace s)
