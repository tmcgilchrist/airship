{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Airship
    ( resourceToWai
    ) where

import           Airship.Decision (flow)
import           Airship.Types (Response(..), ResponseBody(..), eitherResponse)
import           Airship.Resource (Resource)
import           Airship.Route (RoutingSpec, route, runRouter)

import           Data.ByteString (ByteString)
import           Data.Time.Clock (getCurrentTime)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import           Network.Wai (Application, pathInfo)
import qualified Network.Wai as Wai

toWaiResponse :: Response IO -> ByteString -> Wai.Response
toWaiResponse Response{..} trace =
    Wai.responseBuilder _responseStatus headers (fromBody _responseBody)
        where   fromBody (ResponseBuilder b)    = b
                fromBody _                      = mempty
                headers                         = _responseHeaders ++
                                                  [("Airship-Trace", trace)] ++
                                                  [("Airship-Quip",
                                                    "never breaks eye contact")]

resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = runRouter routes
        pInfo = pathInfo req
        (resource, params) = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    (response, trace) <- eitherResponse nowTime params req s (flow resource)
    let traceHeaderValue = traceHeader trace
    respond (toWaiResponse response traceHeaderValue)

traceHeader :: [Text] -> ByteString
traceHeader = encodeUtf8 . T.intercalate ","
