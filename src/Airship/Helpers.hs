{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Airship.Helpers where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text, intercalate)
import           Data.Text.Encoding
import           Network.HTTP.Media
import qualified Network.HTTP.Types  as HTTP
import qualified Network.Wai         as Wai

import           Airship.Decision
import           Airship.Resource
import           Airship.Route
import           Airship.Types
import           Data.Time           (getCurrentTime)


-- | Returns @True@ if the request's Content-Type header is one of the
-- provided media types. If the Content-Type header is not present,
-- this function will return True. (TODO: does that make sense?)
contentTypeMatches :: [MediaType] -> Handler s m Bool
contentTypeMatches validTypes = do
    headers <- Wai.requestHeaders <$> request
    let cType = lookup HTTP.hContentType headers
    return $ case cType of
        Nothing -> True
        Just t  -> isJust $ matchAccept validTypes t

toWaiResponse :: Response IO -> ByteString -> Wai.Response
toWaiResponse Response{..} trace =
    Wai.responseBuilder _responseStatus headers (fromBody _responseBody)
        where   fromBody (ResponseBuilder b)    = b
                fromBody _                      = mempty
                headers                         = _responseHeaders ++
                                                  [("Airship-Trace", trace)] ++
                                                  [("Airship-Quip",
                                                    "never breaks eye contact")]

resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Wai.Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = runRouter routes
        pInfo = Wai.pathInfo req
        (resource, params') = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    (response, trace) <- eitherResponse nowTime params' req s (flow resource)
    let traceHeaderValue = traceHeader trace
    respond (toWaiResponse response traceHeaderValue)

traceHeader :: [Text] -> ByteString
traceHeader = encodeUtf8 . intercalate ","
