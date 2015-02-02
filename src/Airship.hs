module Airship
    ( resourceToWai
    ) where

import Airship.Types (eitherResponse)
import Airship.Resource (Resource, runResource)
import Airship.Route (RoutingSpec, route, runRouter)

import Network.Wai (Application, pathInfo)

resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = runRouter routes
        pInfo = pathInfo req
        resource = route routeMapping pInfo resource404
    response <- eitherResponse req s (runResource resource)
    respond response
