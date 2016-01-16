{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Airship.Internal.Route where

import Airship.Resource

import Data.Monoid
import Data.Foldable (foldr')
import Data.Text (Text)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import Control.Monad.Identity
import Control.Monad.Trans.State (State, evalState, modify, put, state)
import Control.Monad.Writer (Writer, WriterT (..), execWriter, tell)
import Control.Monad.Writer.Class (MonadWriter)

import Data.String (IsString, fromString)

infixl 4 </>
infixl 5 />
infixl 5 </
infixl 3 #>

-- | 'Route's represent chunks of text used to match over URLs.
-- You match hardcoded paths with string literals (and the @-XOverloadedStrings@ extension),
-- captured variables with the 'var' combinator, and wildcards with 'star'.
data Route a =
  Route {
      getRoute :: [BoundOrUnbound]
    -- Allow each 'Route' to consume segments and convert to a value 
    , routeValue :: State [Text] a
    } deriving (Functor)

instance Applicative Route where
  pure a = Route [] (return a)
  (<*>) (Route r1 v1) (Route r2 v2) = Route (r1 <> r2) (v1 <*> v2)

data BoundOrUnbound = Bound Text
                    | Var
                    | RestUnbound deriving (Show)

instance IsString (Route ()) where
    fromString s = Route [Bound (fromString s)] (modify (drop 1))

data RouteResource m =
  forall a. RouteResource (Route a) (Resource a m)

runRouter :: RoutingSpec m a -> [RouteResource m]
runRouter routes = execWriter (getRouter routes)

-- | @a '</>' b@ separates the path components @a@ and @b@ with a slash.
(</>) :: Route (a -> b) -> Route a -> Route b
(</>) = (<*>)

-- | @a '/>' b@ separates the path components @a@ and @b@ with a slash, discarding the value of the first argument
(/>) :: Route () -> Route b -> Route b
(/>) = (*>)

-- | @a '</' b@ separates the path components @a@ and @b@ with a slash, discarding the value of the second argument
(</) :: Route a -> Route () -> Route a
(</) = (<*)

-- | Represents the root resource (@/@). This should usually be the first path declared in a 'RoutingSpec'.
root :: Route ()
root = Route [] (return ())

-- | Captures a named in a route and adds it to the 'routingParams' hashmap under the provided 'Text' value. For example,
--
-- @
--    "blog" '/>' 'var' '</>' 'var'
-- @
--
-- will capture all URLs of the form @\/blog\/$date\/$post@, and add @date@ and @post@ to the 'routingParams'
-- contained within the resource this route maps to.
var :: Route Text
var = Route [Var]
  -- Yuck. :( Note that given the implementation of 'matchesRoute' it should be impossible to fail
  (state (\t -> (head t, drop 1 t)))

-- | Captures a wildcard route. For example,
--
-- @
--    "emcees" '</>' star
-- @
--
-- will match @\/emcees@, @\/emcees/biggie@, @\/emcees\/earl\/vince@, and so on and so forth.
star :: Route ()
star = Route [RestUnbound] (put [] >> return ())

-- | Represents a fully-specified set of routes that map paths (represented as 'Route's) to 'Resource's. 'RoutingSpec's are declared with do-notation, to wit:
--
-- @
--    myRoutes :: RoutingSpec IO ()
--    myRoutes = do
--      root                                 #> myRootResource
--      Blog <$> "blog" '/>' var '</>' var   #> blogPostResource
--      "about"                              #> aboutResource
--      "anything" '/>' star                 #> wildcardResource
-- @
--
newtype RoutingSpec m a = RoutingSpec { getRouter :: Writer [RouteResource m] a }
    deriving (Functor, Applicative, Monad, MonadWriter [RouteResource m])

-- Indicate that a particular 'Route' maps to a given 'Resource'
(#>) :: Monad m => Route p -> Resource p m -> RoutingSpec m ()
k #> v = tell [RouteResource k v]

route :: Monad m => [RouteResource m] -> [Text] -> Resource () m -> (Resource () m, [Text])
route routes pInfo resource404 = foldr' (matchRoute pInfo) (resource404, mempty) routes

matchRoute :: Monad m => [Text] -> RouteResource m -> (Resource () m, [Text]) -> (Resource () m, [Text])
matchRoute paths (RouteResource rSpec resource) (previousMatch, previousMap) =
    case matchRoute' paths rSpec of
      Nothing -> (previousMatch, previousMap)
      Just (p, s) -> (setResourceParams p resource, s)

matchRoute' :: [Text] -> Route a -> Maybe (a, [Text])
matchRoute' paths rSpec =
    matchesRoute paths rSpec >>= \s ->
      return (evalState (routeValue rSpec) paths, s)

matchesRoute :: [Text] -> Route p -> Maybe [Text]
matchesRoute paths spec = matchesRoute' paths (getRoute spec) mempty False where
    -- recursion is over, and we never bailed out to return false, so we match
    matchesRoute' []        []              acc     _   = Just acc
    -- there is an extra part of the path left, and we don't have more matching
    matchesRoute' (_ph:_ptl) []             _       _   = Nothing
    -- we match whatever is left, so it doesn't matter what's left in the path
    matchesRoute' r        (RestUnbound:_) d        _   = Just (d ++ r)
    -- we match a specific string, and it matches this part of the path,
    -- so recur
    matchesRoute' (ph:ptl)  (Bound sh:stt)  dispatch True
        | ph == sh
                                                    = matchesRoute' ptl stt (dispatch ++ [ph]) True
    matchesRoute' (ph:ptl)  (Bound sh:stt)  dispatch False
        | ph == sh
                                                    = matchesRoute' ptl stt dispatch False
    matchesRoute' (_:ptl)   (Var:stt)       acc   _ = matchesRoute' ptl stt acc True
    matchesRoute' _         _               _acc  _ = Nothing
