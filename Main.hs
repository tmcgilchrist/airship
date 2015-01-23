{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( Webmachine(..)
    , Resource(..)
    , finishWith
    , main
    , runWebmachine
    , resourceToWai
    ) where

import Blaze.ByteString.Builder.Char.Utf8 (fromShow)

import Control.Applicative (Applicative)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)

import Network.Wai (Application, Request, Response, responseLBS, responseBuilder, requestMethod, queryString)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (Method, methodGet, status200, status405, status503)

newtype Webmachine a =
    Webmachine { getWebmachine :: ReaderT Request (EitherT Response IO) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadReader Request)

request :: Webmachine Request
request = ask

finishWith :: Response -> Webmachine a
finishWith response = Webmachine $ ReaderT (\_req -> EitherT (return (Left response)))

runWebmachine :: Request -> Webmachine a -> IO (Either Response a)
runWebmachine req w = runEitherT (runReaderT (getWebmachine w) req)

data Resource =
    Resource { allowedMethods   :: Webmachine [Method]
             , serviceAvailable :: Webmachine Bool
             , content          :: Webmachine Response
             }

eitherResponse :: Request -> Webmachine Response -> IO Response
eitherResponse req resource = do
    e <- runWebmachine req resource
    case e of
        (Left r) -> return r
        (Right r) -> return r

runResource :: Resource -> Webmachine Response
runResource Resource{..} = do
    req <- request

    -- bail out earlier if the request method is incorrect
    acceptableMethods <- allowedMethods
    unless (requestMethod req `elem` acceptableMethods) $ finishWith (responseLBS status405 [] "")

    -- bail out early if the service is not available
    available <- serviceAvailable
    unless available $ finishWith (responseLBS status503 [] "")

    -- otherwise return the normal response
    content

resourceToWai :: Resource -> Application
resourceToWai resource req respond = do
    response <- eitherResponse req (runResource resource)
    respond response

-- Resource examples ---------------------------------------------------------
------------------------------------------------------------------------------

myServiceAvailable :: Webmachine Bool
myServiceAvailable = do
    liftIO $ putStrLn "During service available"
    return True

myAllowedMethods :: Webmachine [Method]
myAllowedMethods = return [methodGet]

myContent :: Webmachine Response
myContent = do
    req <- request
    let query = queryString req
        queryBs = fromShow query
    return $ responseBuilder status200 [] queryBs

main :: IO ()
main = do
    let port = 3000
        resource = Resource { allowedMethods    = myAllowedMethods
                            , serviceAvailable  = myServiceAvailable
                            , content           = myContent }

    run port (resourceToWai resource)
