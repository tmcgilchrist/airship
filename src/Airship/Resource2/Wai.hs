module Airship.Resource2.Wai
  ( resourceToWaiT
  ) where

import           Airship.Config
import           Airship.Internal.Helpers
import           Airship.Internal.Route
import           Airship.Resource2.Data
import           Airship.Resource2.Decision
import           Airship.Resource2.Route
import           Airship.Types

import           Data.Time (getCurrentTime)

import qualified Network.Wai as Wai


-- | Given a 'RoutingSpec', a 404 resource, and a user state @s@, construct a WAI 'Application'.
resourceToWaiT :: Monad m => AirshipConfig -> (Request -> m Wai.Response -> IO Wai.Response) -> RoutingSpec m () -> Resource m -> Wai.Application
resourceToWaiT cfg run routes resource404 req respond = do
    let routeMapping = runRouter routes
        pInfo = Wai.pathInfo req
        (resource', (params', matched)) = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    quip <- getQuip
    (=<<) respond . run req $ do
      (response, trace') <- eitherResponse nowTime params' matched req (flow resource')
      return $ toWaiResponse response cfg (traceHeader trace') quip
