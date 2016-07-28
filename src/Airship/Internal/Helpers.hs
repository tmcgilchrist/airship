{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Airship.Internal.Helpers
    ( parseFormData
    , contentTypeMatches
    , redirectTemporarily
    , redirectPermanently
    , resourceToWai
    , resourceToWaiT
    , appendRequestPath
    , lookupParam
    , lookupParam'
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad             (join)
import           Data.ByteString           (ByteString, intercalate)
import qualified Data.ByteString.Lazy      as LB
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif
import           Data.Foldable             (forM_)
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import           Data.Text.Encoding
import           Data.Time                 (getCurrentTime)
import           Lens.Micro                ((^.))
import           Network.HTTP.Media
import qualified Network.HTTP.Types        as HTTP
import qualified Network.Wai               as Wai

import           Network.Wai.Parse
import           System.Random

import           Airship.Config
import           Airship.Headers
import           Airship.Internal.Decision
import           Airship.Internal.Route
import           Airship.Resource
import           Airship.Types

-- | Parse form data uploaded with a @Content-Type@ of either
-- @www-form-urlencoded@ or @multipart/form-data@ to return a
-- list of parameter names and values and a list of uploaded
-- files and their information.
parseFormData :: Request -> IO ([Param], [File LB.ByteString])
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
        Just t  -> isJust $ matchContent validTypes t

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
        headers = traced ++ quipHeader ++ _responseHeaders
        traced  = if cfg^.includeTraceHeader == IncludeHeader
                      then [("Airship-Trace", trace)]
                      else []

        quipHeader  = if cfg^.includeQuipHeader == IncludeHeader
                      then [("Airship-Quip", quip)]
                      else []

-- | Given a 'RoutingSpec', a 404 resource, and a user state @s@, construct a WAI 'Application'.
resourceToWai :: AirshipConfig
              -> RoutingSpec IO ()
              -> ErrorResponses IO
              -> Wai.Application
resourceToWai cfg routes errors =
    resourceToWaiT cfg (const id) routes errors

-- | Given a 'RoutingSpec', an 'ErrorResponses', and a user state @s@, construct a WAI 'Application'.
resourceToWaiT :: Monad m =>
                  AirshipConfig
               -> (Request -> m Wai.Response -> IO Wai.Response)
               -> RoutingSpec m ()
               -> ErrorResponses m
               -> Wai.Application
resourceToWaiT cfg run routes errors req respond = do
    let routeMapping = runRouter routes
        pInfo = Wai.pathInfo req
    quip <- getQuip
    nowTime <- getCurrentTime
    let (er, (ps, matched), r) =
         case route routeMapping pInfo of
             Nothing ->
                 (errors, (mempty, []), return $ Response HTTP.status404 [(HTTP.hContentType, "text/plain")] Empty)
             Just (resource, pm) ->
                 (M.union (errorResponses resource) errors, pm, flow resource)
    respond =<< run req (do
        (response, trace) <- eitherResponse nowTime ps matched req (r >>= errorResponse er)
        return $ toWaiResponse response cfg (traceHeader trace) quip)


-- | If the Response body is Empty the response body is set based on the error responses
--  provided by the application and resource. If the response body is not Empty or
--  there are no error response configured for the status code in the Response then no
--  action is taken. The contents of the 'Webmachine'' response body will be streamed
--  back to the client.
errorResponse :: Monad m =>
                 ErrorResponses m
              -> Response
              -> Webmachine m Response
errorResponse errResps r@Response{..}
    | (HTTP.statusIsClientError _responseStatus ||
       HTTP.statusIsServerError _responseStatus) &&
       isResponseBodyEmpty _responseBody = do
           req <- request
           let reqHeaders = requestHeaders req
               acceptStr = lookup HTTP.hAccept reqHeaders
               errBodies = map dupContentType <$> M.lookup _responseStatus errResps
               mResp = join $ mapAcceptMedia <$> errBodies <*> acceptStr
           forM_ mResp $ \(ct, body) -> do
               putResponseBody =<< body
               addResponseHeader ("Content-Type", renderHeader ct)
           Response
               <$> return _responseStatus
               <*> getResponseHeaders
               <*> getResponseBody
    | otherwise = return r
    where
        isResponseBodyEmpty Empty = True
        isResponseBodyEmpty _ = False
        dupContentType (a, b) = (a, (a, b))


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

traceHeader :: [ByteString] -> ByteString
traceHeader = intercalate ","

-- | Lookup routing parameter and return 500 Internal Server Error if not found.
-- Not finding the paramter usually means the route doesn't match what
-- the resource is expecting.
lookupParam :: Monad m => Text -> Webmachine m Text
lookupParam p = lookupParam' p >>= maybe (halt HTTP.status500) pure

-- | Lookup routing parameter.
lookupParam' :: Monad m => Text -> Webmachine m (Maybe Text)
lookupParam' p = HM.lookup p <$> params
