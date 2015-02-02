{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import qualified Data.ByteString.Lazy as LB
import Data.Monoid
import Data.Foldable (foldr')
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.HashMap.Strict (HashMap, empty, insert)

import Control.Applicative (Applicative)
import Control.Exception.Lifted (IOException, handle)
import Control.Monad (unless)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import Control.Monad.Trans.RWS.Strict (RWST(..), runRWST)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer (Writer, execWriter)

import Network.Wai (Application, Request, Response, pathInfo, responseLBS,
                    responseBuilder, requestMethod)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (Method, methodGet, status200, status405, status500, status503)

import Data.String (IsString, fromString)

-- Monad Control fun
------------------------------------------------------------------------------

newtype Webmachine s m a =
    Webmachine { getWebmachine :: EitherT Response (RWST Request [Integer] s m) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadBase b,
                  MonadReader Request,
                  MonadWriter [Integer],
                  MonadState s)

instance MonadTrans (Webmachine s) where
    lift = Webmachine . EitherT . (>>= return . Right) . lift

instance MonadTransControl (Webmachine s) where
    type StT (Webmachine s) a = StT (RWST Request [Integer] s) (StT (EitherT Response) a)
    liftWith f = Webmachine . liftWith $ \r -> liftWith $ \r' -> f $ r' . r . getWebmachine
    restoreT = Webmachine . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (Webmachine s m) where
  type StM (Webmachine s m) a = ComposeSt (Webmachine s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM  = defaultRestoreM

-- TODO fix the order of args to handler
type Handler s m a = Monad m => Webmachine s m a

-- Functions inside the Webmachine Monad -------------------------------------
------------------------------------------------------------------------------

request :: Handler m s Request
request = ask

state :: Handler s m s
state = get

putState :: s -> Handler s m ()
putState = put

modifyState :: (s -> s) -> Handler s m ()
modifyState = modify

finishWith :: Response -> Handler m s a
finishWith = Webmachine . left

serverError :: Handler m s a
serverError = finishWith (responseLBS status500 [] "")

------------------------------------------------------------------------------
------------------------------------------------------------------------------

runWebmachine :: Monad m => Request -> s -> Handler s m a -> m (Either Response a)
runWebmachine req s w = do
    (e, _, _) <- runRWST (runEitherT (getWebmachine w)) req s
    return e

data Resource s m =
    Resource { allowedMethods   :: Handler s m [Method]
             , serviceAvailable :: Handler s m Bool
             , content          :: Handler s m Response
             }

-- | Grab whichever value 'a' is in Either
both :: Either a a -> a
both = either id id

eitherResponse :: Monad m => Request -> s -> Handler s m Response -> m Response
eitherResponse req s resource = do
    e <- runWebmachine req s resource
    return $ both e

runResource :: Resource s IO -> Handler s IO Response
runResource Resource{..} = do
    req <- request

    let catcher (_e :: IOException) = serverError
    handle catcher $ do
        -- bail out earlier if the request method is incorrect
        acceptableMethods <- allowedMethods
        unless (requestMethod req `elem` acceptableMethods) $ finishWith (responseLBS status405 [] "")

    -- bail out early if the service is not available
    available <- serviceAvailable
    unless available $ finishWith (responseLBS status503 [] "")

    -- otherwise return the normal response
    content

-- Routing
------------------------------------------------------------------------------

defaultResource :: Resource Integer IO
defaultResource = Resource { allowedMethods    = myAllowedMethods
                           , serviceAvailable  = myServiceAvailable
                           , content           = myContent
                           }

(#>) :: Monad m => Route -> Resource s m -> RoutingSpec s m ()
p #> r = tell [(p, r)]


newtype RoutingSpec s m a = RoutingSpec { getRouter :: Writer [(Route, Resource s m)] a }
    deriving (Functor, Applicative, Monad, MonadWriter [(Route, Resource s m)])

resourceWithBody :: LB.ByteString -> Resource Integer IO
resourceWithBody b = defaultResource{ content = return $ responseLBS status200 [] b }

myRoutes :: RoutingSpec Integer IO ()
myRoutes = do
    root                        #> resourceWithBody "root resource"
    "account"                   #> resourceWithBody "account resource"
    "♥"                         #> resourceWithBody "heart resource"
    "account" </> var "name"    #> resourceWithBody "account subresource"

newtype Route = Route { getRoute :: [BoundOrUnbound] } deriving (Show, Monoid)

data BoundOrUnbound = Bound Text
                    | Var Text
                    | RestUnbound deriving (Show)

instance IsString Route where
    fromString s = Route [Bound (fromString s)]

(</>) :: Route -> Route -> Route
(</>) = (<>)

root :: Route
root = Route []

var :: Text -> Route
var t = Route [Var t]

star :: Route
star = Route [RestUnbound]

route :: [(Route, a)] -> [Text] -> a -> a
route routes pInfo resource404 = foldr' (matchRoute pInfo) resource404 routes

matchRoute :: [Text] -> (Route, a) -> a -> a
matchRoute paths (rSpec, resource) previousMatch =
    if isJust (matchesRoute paths rSpec)
    then resource
    else previousMatch

matchesRoute :: [Text] -> Route -> Maybe (HashMap Text Text)
matchesRoute paths spec = matchesRoute' paths (getRoute spec) empty where
    -- recursion is over, and we never bailed out to return false, so we match
    matchesRoute' []        []              acc     = Just acc
    -- there is an extra part of the path left, and we don't have more matching
    matchesRoute' (_ph:_ptl) []             _       = Nothing
    -- we match whatever is left, so it doesn't matter what's left in the path
    matchesRoute' _         (RestUnbound:_) acc     = Just acc
    -- we match a specific string, and it matches this part of the path,
    -- so recur
    matchesRoute' (ph:ptl)  (Bound sh:stt)  acc
        | ph == sh
                                                    = matchesRoute' ptl stt acc
    matchesRoute' (ph:ptl)  (Var t:stt)     acc     = matchesRoute' ptl stt (insert t ph acc)
    matchesRoute' _         _               _acc    = Nothing

resourceToWai :: RoutingSpec s IO () -> Resource s IO -> s -> Application
resourceToWai routes resource404 s req respond = do
    let routeMapping = execWriter (getRouter routes)
        pInfo = pathInfo req
        resource = route routeMapping pInfo resource404
    response <- eitherResponse req s (runResource resource)
    respond response


-- Resource examples ---------------------------------------------------------
------------------------------------------------------------------------------

myServiceAvailable :: Handler s IO Bool
myServiceAvailable = do
    liftIO $ putStrLn "During service available"
    return True

myAllowedMethods :: Handler s m [Method]
myAllowedMethods = return [methodGet]

myContent :: Show s => Handler s m Response
myContent = do
    _req <- request
    s <- get
    let myS = fromShow s
    return $ responseBuilder status200 [] myS

main :: IO ()
main = do
    let port = 3000
        s = 5 :: Integer
        routes = myRoutes

    putStrLn "Listening on port 3000"
    run port (resourceToWai routes defaultResource s)
