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

module Airship where

import Airship.Types

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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Trans.Either (left)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer (Writer, execWriter)

import Network.Wai (Application, Request, Response, pathInfo, responseLBS,
                    responseBuilder, requestMethod)
import Network.HTTP.Types (Method, methodGet, status200, status405, status500, status503)

import Data.String (IsString, fromString)

-- Monad Control fun
------------------------------------------------------------------------------


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
