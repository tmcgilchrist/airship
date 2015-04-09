{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Airship.Internal.Helpers where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Data.ByteString     (ByteString)
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif
import           Data.Text           (Text, intercalate)
import           Data.Text.Encoding
import           Data.Time           (getCurrentTime)
import           Network.HTTP.Media
import qualified Network.HTTP.Types  as HTTP
import qualified Network.Wai         as Wai
import           System.Random

import           Airship.Internal.Decision
import           Airship.Resource
import           Airship.Types
import           Airship.Internal.Route


-- | Returns @True@ if the request's @Content-Type@ header is one of the
-- provided media types. If the @Content-Type@ header is not present,
-- this function will return True.
contentTypeMatches :: [MediaType] -> Handler s m Bool
contentTypeMatches validTypes = do
    headers <- requestHeaders <$> request
    let cType = lookup HTTP.hContentType headers
    return $ case cType of
        Nothing -> True
        Just t  -> isJust $ matchAccept validTypes t

-- | Construct an Airship 'Request' from a WAI request.
fromWaiRequest :: Wai.Request -> Request IO
fromWaiRequest req = Request
    { requestMethod = Wai.requestMethod req
    , httpVersion = Wai.httpVersion req
    , rawPathInfo = Wai.rawPathInfo req
    , rawQueryString = Wai.rawQueryString req
    , requestHeaders = Wai.requestHeaders req
    , isSecure = Wai.isSecure req
    , remoteHost = Wai.remoteHost req
    , pathInfo = Wai.pathInfo req
    , queryString = Wai.queryString req
    , requestBody = Wai.requestBody req
    , requestBodyLength = Wai.requestBodyLength req
    , requestHeaderHost = Wai.requestHeaderHost req
    , requestHeaderRange = Wai.requestHeaderRange req
    }

toWaiResponse :: Response IO -> ByteString -> ByteString -> Wai.Response
toWaiResponse Response{..} trace quip =
    Wai.responseBuilder _responseStatus headers (fromBody _responseBody)
        where   fromBody (ResponseBuilder b)    = b
                fromBody _                      = mempty
                headers                         = _responseHeaders ++
                                                  [("Airship-Trace", trace)] ++
                                                  [("Airship-Quip", quip)]

-- | Given a 'RoutingSpec', a 404 resource, and a user state @s@, construct a WAI 'Application'.
resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Wai.Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = runRouter routes
        pInfo = Wai.pathInfo req
        airshipReq = fromWaiRequest req
        (resource, params') = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    quip <- getQuip
    (response, trace) <- eitherResponse nowTime params' airshipReq s (flow resource)
    let traceHeaderValue = traceHeader trace
    respond (toWaiResponse response traceHeaderValue quip)

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
                ]

traceHeader :: [Text] -> ByteString
traceHeader = encodeUtf8 . intercalate ","
