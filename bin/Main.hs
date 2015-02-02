{-# LANGUAGE OverloadedStrings #-}

module Main where

import Airship (resourceToWai)
import Airship.Resource (Resource(..), defaultResource)
import Airship.Route (RoutingSpec, (#>), (</>), var, root)
import qualified Data.ByteString.Lazy as LB
import Network.Wai.Handler.Warp (run)
import Network.Wai (responseLBS)
import Network.HTTP.Types (status200)

resourceWithBody :: LB.ByteString -> Resource Integer IO
resourceWithBody b = defaultResource{ content = return $ responseLBS status200 [] b }

myRoutes :: RoutingSpec Integer IO ()
myRoutes = do
    root                        #> resourceWithBody "root resource"
    "account"                   #> resourceWithBody "account resource"
    "♥"                         #> resourceWithBody "heart resource"
    "account" </> var "name"    #> resourceWithBody "account subresource"

main :: IO ()
main = do
    let port = 3000
        s = 5 :: Integer
        routes = myRoutes

    putStrLn "Listening on port 3000"
    run port (resourceToWai routes defaultResource s)
