{-# LANGUAGE RecordWildCards #-}

module Airship
    ( resourceToWai
    ) where

import Airship.Decision (flow)
import Airship.Types (Response(..), ResponseBody(..), eitherResponse)
import Airship.Resource (Resource)
import Airship.Route (RoutingSpec, route, runRouter)

import Data.Time.Clock (getCurrentTime)
import Data.Monoid (mempty)

import Network.Wai (Application, pathInfo)
import qualified Network.Wai as Wai

toWaiResponse :: Response IO -> Wai.Response
toWaiResponse Response{..} = Wai.responseBuilder _responseStatus _responseHeaders (fromBody _responseBody)
    where   fromBody (ResponseBuilder b)    = b
            fromBody _                      = mempty

resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = runRouter routes
        pInfo = pathInfo req
        (resource, params) = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    (response, trace) <- eitherResponse nowTime params req s (flow resource)
    print trace
    respond (toWaiResponse response)
