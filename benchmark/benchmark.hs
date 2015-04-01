{-# LANGUAGE OverloadedStrings #-}

module Main where

import Airship
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Trans.State.Strict (State, evalState)
import Criterion.Main (Benchmark, defaultMain, bgroup, bench, nf)

type StateEffect = State ()
type UserState = ()

myRoutes :: RoutingSpec UserState StateEffect ()
myRoutes = root #> myResource

myResource :: Resource UserState StateEffect
myResource = defaultResource { contentTypesProvided =
                                return [ ( "text/plain"
                                         , return (ResponseBuilder (fromHtmlEscapedText "hello, world"))
                                         )
                                       ] }

runRequest :: RoutingSpec UserState StateEffect ()
           -> Resource UserState StateEffect
           -> UserState
           -> Request StateEffect
           -> UTCTime
           -> StateEffect [Text]
runRequest routes resource404 s req time = do
    let routeMapping = runRouter routes
        pInfo = pathInfo req
        (resource, params') = route routeMapping pInfo resource404
    (_response, trace) <- eitherResponse time params' req s (flow resource)
    return trace

benchmarks :: UTCTime -> [Benchmark]
benchmarks t =
    [ bench "one" $ nf (\x -> evalState (runRequest myRoutes myResource () defaultRequest x) ()) t ]

main :: IO ()
main = do
    now <- getCurrentTime
    defaultMain [bgroup "Benchmark" (benchmarks now)]
