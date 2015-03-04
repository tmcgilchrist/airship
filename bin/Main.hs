{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Airship (resourceToWai)
import           Airship.Resource (Resource(..), defaultResource, singletonContentType)
import           Airship.Route (RoutingSpec, (#>), (</>), var, root)
import           Airship.Types (ResponseBody(..), request, getState, params)

import           Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import           Control.Applicative ((<$>))
import           Control.Concurrent.MVar
import           Control.Monad.Trans (liftIO)

import           Data.HashMap.Strict (HashMap, empty, insert, lookupDefault)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Monoid ((<>))
import           Data.Text(Text, pack)

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (pathInfo, strictRequestBody, requestHeaders)
import           Network.Wai.Handler.Warp (run)

newtype State = State { _getState :: MVar (HashMap Text Integer) }

resourceWithBody :: Text -> Resource State IO
resourceWithBody t = defaultResource{ contentTypesProvided = return $
                                        singletonContentType "text/plain" t
                                    }

accountResource :: Resource State IO
accountResource = defaultResource
    { allowedMethods = return [ HTTP.methodGet
                              , HTTP.methodHead
                              , HTTP.methodPut
                              ]
    , knownContentType = do
        req <- request
        let reqHeaders = requestHeaders req
            cType = lookup HTTP.hContentType reqHeaders
        return (maybe True (== "text/plain") cType)

    , contentTypesProvided = do
        currentPath <- pathInfo <$> request
        s <- getState
        m <- liftIO (readMVar (_getState s))
        let accountName = last currentPath
            val = lookupDefault 0 accountName m
        ps <- params
        liftIO $ print ps
        return [("text/plain", ResponseBuilder (fromHtmlEscapedText (pack (show val) <> "\n")))]

    , contentTypesAccepted = return [("text/plain", do
        req <- request
        s <- getState
        body <- liftIO (strictRequestBody req)
        let currentPath = pathInfo req
            accountName = last currentPath
            val = read (unpack body)
        liftIO (modifyMVar_ (_getState s) (return . insert accountName val))
        return ()
    )]
    }

myRoutes :: RoutingSpec State IO ()
myRoutes = do
    root                        #> resourceWithBody "Just the root resource"
    "account" </> var "name"    #> accountResource

main :: IO ()
main = do
    let port = 3000
        routes = myRoutes
        resource404 = defaultResource

    mvar <- newMVar empty
    let s = State mvar
    putStrLn "Listening on port 3000"
    run port (resourceToWai routes resource404 s)
