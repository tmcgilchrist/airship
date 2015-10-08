{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Airship
import           Airship.Resource.Static            (StaticOptions (..),
                                                     staticResource)

import           Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                ((<$>))
#endif
import           Control.Concurrent.MVar
import           Control.Monad.State                hiding (State)

import qualified Data.ByteString.Lazy               as LB
import           Data.ByteString.Lazy.Char8         (unpack)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict                as HM
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text, pack)
import           Data.Time.Clock

import qualified Network.HTTP.Types                 as HTTP
import           Network.Wai.Handler.Warp           (defaultSettings,
                                                     runSettings, setHost,
                                                     setPort)

-- ***************************************************************************
-- Helpers
-- ***************************************************************************

getBody :: Monad m => Webmachine m LB.ByteString
getBody = do
    req <- request
    lift (entireRequestBody req)

readBody :: MonadIO m => Webmachine m Integer
readBody = read . unpack <$> getBody

routingParam :: Monad m => Text -> Webmachine m Text
routingParam t = do
    p <- params
    return (p HM.! t)

newtype State = State { _getState :: MVar (HashMap Text Integer) }

resourceWithBody :: (MonadIO m, MonadState State m) => Text -> Resource m
resourceWithBody t = defaultResource { contentTypesProvided = return [("text/plain", return (escapedResponse t))]
                                     , lastModified = Just <$> liftIO getCurrentTime
                                     , generateETag = return $ Just $ Strong "abc123"
                                     }

accountResource :: (MonadIO m, MonadState State m) => Resource m
accountResource = defaultResource
    { allowedMethods = return [ HTTP.methodGet
                              , HTTP.methodHead
                              , HTTP.methodPost
                              , HTTP.methodPut
                              ]
    , knownContentType = contentTypeMatches ["text/plain"]

    , contentTypesProvided = do
        let textAction = do
                s <- lift get
                m <- liftIO (readMVar (_getState s))
                accountNameM <- HM.lookup "name" <$> params
                let val = fromMaybe 0 (accountNameM >>= flip HM.lookup m)
                return $ ResponseBuilder (fromHtmlEscapedText
                                                (pack (show val) <> "\n"))
        return [("text/plain", textAction)]

    , allowMissingPost = return False

    , lastModified = Just <$> liftIO getCurrentTime

    , resourceExists = do
        accountName <- routingParam "name"
        s <- lift get
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

postPutStates :: (MonadIO m, MonadState State m) => Webmachine m (Integer, Text, State)
postPutStates = do
    val <- readBody
    accountName <- routingParam "name"
    s <- lift get
    return (val, accountName, s)

myRoutes :: Resource (StateT State IO) -> RoutingSpec (StateT State IO) ()
myRoutes static = do
    root                        #> resourceWithBody "Just the root resource"
    "account" </> var "name"    #> accountResource
    "static"  </> star          #> static

main :: IO ()
main = do
    static <- staticResource Cache "assets"
    let port = 3000
        host = "127.0.0.1"
        settings = setPort port (setHost host defaultSettings)
        routes = myRoutes static
        response404 = escapedResponse "<html><head></head><body><h1>404 Not Found</h1></body></html>"
        resource404 = defaultResource { resourceExists = return False
                                      , contentTypesProvided = return
                                            [ ( "text/html"
                                              , return response404
                                              )
                                            ]
                                      }

    mvar <- newMVar HM.empty
    let s = State mvar
    putStrLn "Listening on port 3000"
    runSettings settings (resourceToWaiT defaultAirshipConfig (flip evalStateT s) routes resource404)
