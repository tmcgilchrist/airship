{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Airship.Route where

import Airship.Resource

import Data.Monoid
import Data.Foldable (foldr')
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.HashMap.Strict (HashMap, empty, insert)

import Control.Applicative (Applicative)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer (Writer)

import Data.String (IsString, fromString)


(#>) :: Monad m => Route -> Resource s m -> RoutingSpec s m ()
p #> r = tell [(p, r)]

newtype RoutingSpec s m a = RoutingSpec { getRouter :: Writer [(Route, Resource s m)] a }
    deriving (Functor, Applicative, Monad, MonadWriter [(Route, Resource s m)])

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
