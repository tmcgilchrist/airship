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

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Monoid ((<>))
import           Data.Text(Text, pack)

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (strictRequestBody, requestHeaders)
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
        s <- getState
        m <- liftIO (readMVar (_getState s))
        accountNameM <- HM.lookup "name" <$> params
        let val = fromMaybe 0 (accountNameM >>= flip HM.lookup m)
        return [("text/plain", ResponseBuilder (fromHtmlEscapedText (pack (show val) <> "\n")))]

    , contentTypesAccepted = return [("text/plain", do
        req <- request
        s <- getState
        body <- liftIO (strictRequestBody req)
        accountName <- fromJust <$> HM.lookup "name" <$> params
        let val = read (unpack body)
        liftIO (modifyMVar_ (_getState s) (return . HM.insert accountName val))
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

    mvar <- newMVar HM.empty
    let s = State mvar
    putStrLn "Listening on port 3000"
    run port (resourceToWai routes resource404 s)
