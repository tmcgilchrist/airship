module Airship where

import Airship.Resource
import Airship.Route

import Control.Monad.Writer (execWriter)

import Network.Wai (Application, pathInfo)

resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = execWriter (getRouter routes)
        pInfo = pathInfo req
        resource = route routeMapping pInfo resource404
    response <- eitherResponse req s (runResource resource)
    respond response
