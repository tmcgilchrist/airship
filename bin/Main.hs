module Main where

import Airship
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
        s = 5 :: Integer
        routes = myRoutes

    putStrLn "Listening on port 3000"
    run port (resourceToWai routes defaultResource s)
