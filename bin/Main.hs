{-# LANGUAGE OverloadedStrings #-}

module Main where

import Airship (resourceToWai)
import Airship.Resource (Resource(..), defaultResource, singletonContentType)
import Airship.Route (RoutingSpec, (#>), (</>), var, root)

import Data.Text(Text)

import Network.Wai.Handler.Warp (run)

resourceWithBody :: Text -> Resource Integer IO
resourceWithBody t = defaultResource{ contentTypesProvided = return $
                                        singletonContentType "text/html" t
                                    }

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
