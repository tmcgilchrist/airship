{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Airship.Internal.Helpers where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LazyBS
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif
import           Data.Text                 (Text, intercalate, unpack)
import           Data.Text.Encoding
import           Data.Time                 (getCurrentTime)
import           Lens.Micro                ((^.))
import           Network.HTTP.Media
import qualified Network.HTTP.Types        as HTTP
import qualified Network.Wai               as Wai
import           Network.Wai.Parse
import           System.Random
import           Text.Read

import           Airship.Config
import           Airship.Headers
import           Airship.Internal.Decision
import           Airship.Internal.Route
import           Airship.Resource
import           Airship.Types

-- | As 'readParamMaybe', except this function eliminates the @Maybe@
-- value by invoking the provided @Handler@ should reading the provided
-- parameter fail.
readParam :: Read a => Text -> Handler s m a -> Handler s m a
readParam p def = readParamMaybe p >>= maybe def return

-- | Reads the parameter of the provided name and attempt to convert
-- it to the desired 'Read'-implementing type. If the parameter is not
-- found or cannot be read successfully, @Nothing@ is returned.
readParamMaybe :: Read a => Text -> Handler s m (Maybe a)
readParamMaybe p = do
    mStr <- fmap unpack <$> HM.lookup p <$> params
    return $ mStr >>= readMaybe

-- | Parse form data uploaded with a @Content-Type@ of either
-- @www-form-urlencoded@ or @multipart/form-data@ to return a
-- list of parameter names and values and a list of uploaded
-- files and their information.
parseFormData :: Request -> IO ([Param], [File LazyBS.ByteString])
parseFormData r = parseRequestBody lbsBackEnd r

-- | Returns @True@ if the request's @Content-Type@ header is one of the
-- provided media types. If the @Content-Type@ header is not present,
-- this function will return True.
contentTypeMatches :: Monad m => [MediaType] -> Webmachine m Bool
contentTypeMatches validTypes = do
    headers <- requestHeaders <$> request
    let cType = lookup HTTP.hContentType headers
    return $ case cType of
        Nothing -> True
        Just t  -> isJust $ matchAccept validTypes t

-- | Issue an HTTP 302 (Found) response, with `location' as the destination.
redirectTemporarily :: Monad m => ByteString -> Webmachine m a
redirectTemporarily location =
    addResponseHeader ("Location", location) >> halt HTTP.status302

-- | Issue an HTTP 301 (Moved Permantently) response,
-- with `location' as the destination.
redirectPermanently :: Monad m => ByteString -> Webmachine m a
redirectPermanently location =
    addResponseHeader ("Location", location) >> halt HTTP.status301

toWaiResponse :: Response -> AirshipConfig -> ByteString -> ByteString -> Wai.Response
toWaiResponse Response{..} cfg trace quip =
    case _responseBody of
        (ResponseBuilder b) ->
            Wai.responseBuilder _responseStatus headers b
        (ResponseFile path part) ->
            Wai.responseFile _responseStatus headers path part
        (ResponseStream streamer) ->
            Wai.responseStream _responseStatus headers streamer
        Empty ->
            Wai.responseBuilder _responseStatus headers mempty
    where
        headers = traced ++ [("Airship-Quip", quip)] ++ _responseHeaders
        traced  = if cfg^.includeTraceHeader == IncludeHeader
                      then [("Airship-Trace", trace)]
                      else []

-- | Given a 'RoutingSpec', a 404 resource, and a user state @s@, construct a WAI 'Application'.
resourceToWai :: AirshipConfig -> RoutingSpec IO () -> Resource IO -> Wai.Application
resourceToWai cfg routes resource404 req respond = do
    let routeMapping = runRouter routes
        pInfo = Wai.pathInfo req
        (resource, (params', matched)) = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    quip <- getQuip
    (response, trace) <- eitherResponse nowTime params' matched req (flow resource)
    respond (toWaiResponse response cfg (traceHeader trace) quip)

getQuip :: IO ByteString
getQuip = do
  idx <- randomRIO (0, length quips - 1)
  return $ quips !! idx
  where quips = [ "never breaks eye contact"
                , "blame me if inappropriate"
                , "firm pat on the back"
                , "sharkfed"
                , "$300,000 worth of cows"
                , "RB_GC_GUARD"
                , "evacuation not done in time"
                , "javascript doesn't have integers"
                , "WARNING: ulimit -n is 1024"
                , "shut it down"
                ]

traceHeader :: [Text] -> ByteString
traceHeader = encodeUtf8 . intercalate ","
