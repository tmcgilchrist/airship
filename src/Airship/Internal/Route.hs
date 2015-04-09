{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Airship.Internal.Route where

import Airship.Resource

import Data.Monoid
import Data.Foldable (foldr')
import Data.Text (Text)
import Data.HashMap.Strict (HashMap, insert)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (MonadWriter, tell)

import Data.String (IsString, fromString)

-- | 'Route's represent chunks of text used to match over URLs.
-- You match hardcoded paths with string literals (and the @-XOverloadedStrings@ extension),
-- named variables with the 'var' combinator, and wildcards with 'star'.
newtype Route = Route { getRoute :: [BoundOrUnbound] } deriving (Show, Monoid)

data BoundOrUnbound = Bound Text
                    | Var Text
                    | RestUnbound deriving (Show)

instance IsString Route where
    fromString s = Route [Bound (fromString s)]

runRouter :: RoutingSpec s m a -> [(Route, Resource s m)]
runRouter routes = execWriter (getRouter routes)

-- | Used in 'RoutingSpec' declarations to indicate that a particular 'Route' maps
-- to a given 'Resource'.
(#>) :: Monad m => Route -> Resource s m -> RoutingSpec s m ()
p #> r = tell [(p, r)]

-- | @a '</>' b@ separates the path components @a@ and @b@ with a slash.
-- This is actually just a synonym for 'mappend'.
(</>) :: Route -> Route -> Route
(</>) = (<>)

-- | Represents the root resource (@/@). This should usually be the first path declared in a 'RoutingSpec'.
root :: Route
root = Route []

-- | Captures a named in a route and adds it to the 'routingParams' hashmap under the provided 'Text' value. For example,
--
-- @
--    "blog" '</>' 'var' "date" '</>' 'var' "post"
-- @
--
-- will capture all URLs of the form @\/blog\/$date\/$post@, and add @date@ and @post@ to the 'routingParams'
-- contained within the resource this route maps to.
var :: Text -> Route
var t = Route [Var t]

-- | Captures a wildcard route. For example,
--
-- @
--    "emcees" '</>' star
-- @
--
-- will match @\/emcees@, @\/emcees/biggie@, @\/emcees\/earl\/vince@, and so on and so forth.
star :: Route
star = Route [RestUnbound]

-- | Represents a fully-specified set of routes that map paths (represented as 'Route's) to 'Resource's. 'RoutingSpec's are declared with do-notation, to wit:
--
-- @
--    myRoutes :: RoutingSpec MyState IO ()
--    myRoutes = do
--      root                                 #> myRootResource
--      "blog" '</>' var "date" '</>' var "post" #> blogPostResource
--      "about"                              #> aboutResource
--      "anything" '</>' star                  #> wildcardResource
-- @
--
newtype RoutingSpec s m a = RoutingSpec { getRouter :: Writer [(Route, Resource s m)] a }
    deriving (Functor, Applicative, Monad, MonadWriter [(Route, Resource s m)])


route :: [(Route, a)] -> [Text] -> a -> (a, HashMap Text Text)
route routes pInfo resource404 = foldr' (matchRoute pInfo) (resource404, mempty) routes

matchRoute :: [Text] -> (Route, a) -> (a, HashMap Text Text) -> (a, HashMap Text Text)
matchRoute paths (rSpec, resource) (previousMatch, previousMap) =
    case matchesRoute paths rSpec of
      Nothing -> (previousMatch, previousMap)
      Just m  -> (resource, m)

matchesRoute :: [Text] -> Route -> Maybe (HashMap Text Text)
matchesRoute paths spec = matchesRoute' paths (getRoute spec) mempty where
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
