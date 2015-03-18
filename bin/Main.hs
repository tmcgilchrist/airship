{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Airship (resourceToWai)
import           Airship.Helpers
import           Airship.Resource ( Resource(..)
                                  , PostResponse(..)
                                  , defaultResource
                                  , singletonContentType
                                  )
import           Airship.Route (RoutingSpec, (#>), (</>), var, root)
import           Airship.Types ( ResponseBody(..)
                               , Handler
                               , request
                               , getState
                               , params
                               )

import           Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

import           Control.Applicative ((<$>))
import           Control.Concurrent.MVar
import           Control.Monad.Trans (liftIO)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text(Text, pack)

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (strictRequestBody)
import           Network.Wai.Handler.Warp ( runSettings
                                          , defaultSettings
                                          , setPort
                                          , setHost
                                          )

-- ***************************************************************************
-- Helpers
-- ***************************************************************************

getBody :: Handler s IO LB.ByteString
getBody = do
    req <- request
    liftIO (strictRequestBody req)

readBody :: Handler s IO Integer
readBody = read . unpack <$> getBody

routingParam :: Text -> Handler s m Text
routingParam t = do
    p <- params
    return (p HM.! t)

newtype State = State { _getState :: MVar (HashMap Text Integer) }

resourceWithBody :: Text -> Resource State IO
resourceWithBody t = defaultResource{ contentTypesProvided = return $
                                        singletonContentType "text/plain" t
                                    }

accountResource :: Resource State IO
accountResource = defaultResource
    { allowedMethods = return [ HTTP.methodGet
                              , HTTP.methodHead
                              , HTTP.methodPost
                              , HTTP.methodPut
                              ]
    , knownContentType = contentTypeMatches ["text/plain"]

    , contentTypesProvided = do
        s <- getState
        m <- liftIO (readMVar (_getState s))
        accountNameM <- HM.lookup "name" <$> params
        let val = fromMaybe 0 (accountNameM >>= flip HM.lookup m)
        return [("text/plain", ResponseBuilder (fromHtmlEscapedText (pack (show val) <> "\n")))]

    , allowMissingPost = return False

    , resourceExists = do
        accountName <- routingParam "name"
        s <- getState
        m <- liftIO (readMVar (_getState s))
        return $ HM.member accountName m

    -- POST'ing to this resource adds the integer to the current value
    , processPost = return (PostProcess $ do
        (val, accountName, s) <- postPutStates
        liftIO (modifyMVar_ (_getState s) (return . HM.insertWith (+) accountName val))
        return ()
        )

    , contentTypesAccepted = return [("text/plain", do
        (val, accountName, s) <- postPutStates
        liftIO (modifyMVar_ (_getState s) (return . HM.insert accountName val))
        return ()
    )]
    }

postPutStates :: Handler State IO (Integer, Text, State)
postPutStates = do
    val <- readBody
    accountName <- routingParam "name"
    s <- getState
    return (val, accountName, s)


myRoutes :: RoutingSpec State IO ()
myRoutes = do
    root                        #> resourceWithBody "Just the root resource"
    "account" </> var "name"    #> accountResource

main :: IO ()
main = do
    let port = 3000
        host = "127.0.0.1"
        settings = setPort port (setHost host defaultSettings)
        routes = myRoutes
        resource404 = defaultResource

    mvar <- newMVar HM.empty
    let s = State mvar
    putStrLn "Listening on port 3000"
    runSettings settings (resourceToWai routes resource404 s)
