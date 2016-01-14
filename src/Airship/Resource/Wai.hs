module Airship.Resource.Wai
  ( resourceToWai
  , resourceToWaiT
  ) where

import           Airship.Config
import           Airship.Internal.Route
import           Airship.Internal.Helpers
import           Airship.Resource
import           Airship.Resource.Decision
import           Airship.Resource.Route
import           Airship.Types

import           Data.Time                 (getCurrentTime)

import qualified Network.Wai               as Wai


-- | Given a 'RoutingSpec', a 404 resource, and a user state @s@, construct a WAI 'Application'.
resourceToWai :: AirshipConfig -> RoutingSpec IO () -> Resource IO -> Wai.Application
resourceToWai cfg routes resource404 =
  resourceToWaiT cfg (const id) routes resource404

-- | Given a 'RoutingSpec', a 404 resource, and a user state @s@, construct a WAI 'Application'.
resourceToWaiT :: Monad m => AirshipConfig -> (Request -> m Wai.Response -> IO Wai.Response) -> RoutingSpec m () -> Resource m -> Wai.Application
resourceToWaiT cfg run routes resource404 req respond = do
    let routeMapping = runRouter routes
        pInfo = Wai.pathInfo req
        (resource, (params', matched)) = route routeMapping pInfo resource404
    nowTime <- getCurrentTime
    quip <- getQuip
    (=<<) respond . run req $ do
      (response, trace) <- eitherResponse nowTime params' matched req (flow resource)
      return $ toWaiResponse response cfg (traceHeader trace) quip
