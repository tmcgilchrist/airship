{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Basic2 where

import           Airship2

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
import           Data.List.NonEmpty                 (NonEmpty (..))
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

getBody :: MonadIO m => Webmachine m LB.ByteString
getBody = do
    req <- request
    liftIO (entireRequestBody req)

readBody :: MonadIO m => Webmachine m Integer
readBody = read . unpack <$> getBody

newtype State = State { _getState :: MVar (HashMap Text Integer) }

data Plain = Plain


resourceWithBody :: (MonadIO m, MonadState State m) => Text -> Resource m
resourceWithBody t =
  resource
    resourceDesc { contentTypesProvided = ("text/plain", Plain) :| [] } .
    return $ Right $ \_ accept -> do
      now <- liftIO getCurrentTime
      return . ResourceExists (CacheData (Just now) (Just $ Strong "abc123")) $ \case
        ROther run -> case accept of
          Plain -> run . ok $ escapedResponse t
        _ -> lift serverError

accountResource :: (MonadIO m, MonadState State m) => Resource m
accountResource = do
    let rd = resourceDesc {
            allowedMethods = [HTTP.methodGet, HTTP.methodHead, HTTP.methodPost, HTTP.methodPut]
          , contentTypesAccepted = ("text/plain", Plain) :| []
          , contentTypesProvided = ("text/plain", Plain) :| []
          }
    resource rd . return . Right $ \_ accept -> do
        accountName <- lookupParam "name"
        s <- lift get
        m <- liftIO (readMVar (_getState s))
        now <- liftIO getCurrentTime
        let cache = noCacheData { cacheModified = Just now }
        return $ case HM.lookup accountName m of
            Nothing ->
                ResourceNotFound $ \case
                    MPost run ->
                        run MPostNotFound
                    MOther run ->
                        run MOtherNotFound
                    MPut run -> do
                        val <- lift readBody
                        liftIO (modifyMVar_ (_getState s) (return . HM.insert accountName val))
                        run . MPutOk cache . ok $ Empty
            Just val ->
                ResourceExists cache $ \case
                    ROther run -> case accept of
                        Plain ->
                            run . ok $ ResponseBuilder (fromHtmlEscapedText (pack (show val) <> "\n"))
                    RPut run -> case accept of
                        Plain -> do
                            val' <- lift readBody
                            liftIO (modifyMVar_ (_getState s) (return . HM.insert accountName val'))
                            run . RPutOk . ok $ Empty
                    -- POST'ing to this resource adds the integer to the current value
                    RPost run -> case accept of
                        Plain -> do
                            val' <- lift readBody
                            liftIO (modifyMVar_ (_getState s) (return . HM.insertWith (+) accountName val'))
                            run . RPostOk . ok $ Empty
                    RDelete _ -> lift serverError

resource404 :: Monad m => Resource m
resource404 = do
  -- TODO The types now show that this wasn't used
  let _response404 = escapedResponse "<html><head></head><body><h1>404 Not Found</h1></body></html>"
  resource
    resourceDesc { contentTypesProvided = ("text/html", ()) :| [] } .
    return $ Right $ \_ _ ->
      return . ResourceNotFound $ \case
        MOther run -> run MOtherNotFound
        _ -> lift serverError

myRoutes :: RoutingSpec (StateT State IO) ()
myRoutes = do
    root                        #> resourceWithBody "Just the root resource"
    "account" </> var "name"    #> accountResource

main :: IO ()
main = do
    let port = 3000
        host = "127.0.0.1"
        settings = setPort port (setHost host defaultSettings)

    mvar <- newMVar HM.empty
    let s = State mvar
    putStrLn "Listening on port 3000"
    runSettings settings (resourceToWaiT defaultAirshipConfig (const $ flip evalStateT s) myRoutes resource404)
