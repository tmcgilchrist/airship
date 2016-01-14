{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Airship.Internal.Route where

import Data.Monoid
import Data.Foldable (foldr')
import Data.Text (Text)
import Data.HashMap.Strict (HashMap, insert)

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


route :: [(Route, a)] -> [Text] -> a -> (a, (HashMap Text Text, [Text]))
route routes pInfo resource404 = foldr' (matchRoute pInfo) (resource404, (mempty, mempty)) routes

matchRoute :: [Text] -> (Route, a) -> (a, (HashMap Text Text, [Text])) -> (a, (HashMap Text Text, [Text]))
matchRoute paths (rSpec, resource) (previousMatch, previousMap) =
    case matchesRoute paths rSpec of
      Nothing -> (previousMatch, previousMap)
      Just m  -> (resource, m)

matchesRoute :: [Text] -> Route -> Maybe (HashMap Text Text, [Text])
matchesRoute paths spec = matchesRoute' paths (getRoute spec) (mempty, mempty) False where
    -- recursion is over, and we never bailed out to return false, so we match
    matchesRoute' []        []              acc     _   = Just acc
    -- there is an extra part of the path left, and we don't have more matching
    matchesRoute' (_ph:_ptl) []             _       _   = Nothing
    -- we match whatever is left, so it doesn't matter what's left in the path
    matchesRoute' r        (RestUnbound:_) (h, d)  _   = Just (h, d ++ r)
    -- we match a specific string, and it matches this part of the path,
    -- so recur
    matchesRoute' (ph:ptl)  (Bound sh:stt)  (h, dispatch) True
        | ph == sh
                                                    = matchesRoute' ptl stt (h, dispatch ++ [ph]) True
    matchesRoute' (ph:ptl)  (Bound sh:stt)  (h, dispatch) False
        | ph == sh
                                                    = matchesRoute' ptl stt (h, dispatch) False
    matchesRoute' (ph:ptl)  (Var t:stt)     acc   _ = matchesRoute' ptl stt (insert t ph (fst acc), snd acc) True
    matchesRoute' _         _               _acc  _ = Nothing
