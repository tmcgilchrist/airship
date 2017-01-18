{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_HADDOCK hide                #-}

module Airship.Internal.Route
    ( RoutingSpec
    , Route
    , RouteLeaf
    , RoutedResource(..)
    , Trie
    , root
    , var
    , star
    , (</>)
    , (#>)
    , (#>=)
    , runRouter
    , route
    , routeText
    ) where

import           Airship.Resource           as Resource

import           Control.Monad.Writer.Class (MonadWriter, tell)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as BC8
import           Data.HashMap.Strict        (HashMap, fromList)
import qualified Data.List                  as L (foldl')
import           Data.Maybe                 (isNothing)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T (intercalate, cons)
import           Data.Text.Encoding         (encodeUtf8, decodeUtf8)
import           Data.Trie                  (Trie)
import qualified Data.Trie                  as Trie


#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Writer       (Writer, WriterT (..), execWriter)

import           Data.String                (IsString, fromString)

-- | 'Route's represent chunks of text used to match over URLs.
-- You match hardcoded paths with string literals (and the @-XOverloadedStrings@ extension),
-- named variables with the 'var' combinator, and wildcards with 'star'.
newtype Route = Route { getRoute :: [BoundOrUnbound] } deriving (Show, Monoid)

routeText :: Route -> Text
routeText (Route parts) =
    T.cons '/' $ T.intercalate "/" ((boundOrUnboundText <$> parts))

data BoundOrUnbound = Bound Text
                    | Var Text
                    | RestUnbound deriving (Show)


boundOrUnboundText :: BoundOrUnbound -> Text
boundOrUnboundText (Bound t) = t
boundOrUnboundText (Var t) = ":" <> t
boundOrUnboundText (RestUnbound) = "*"




instance IsString Route where
    fromString s = Route [Bound (fromString s)]


data RoutedResource m
    = RoutedResource Route (Resource m)


data RouteLeaf m = RouteMatch (RoutedResource m) [Text]
                 | RVar
                 | RouteMatchOrVar (RoutedResource m) [Text]
                 | Wildcard (RoutedResource m)


-- | Turns the list of routes in a 'RoutingSpec' into a 'Trie' for efficient
-- routing
runRouter :: RoutingSpec m a -> Trie (RouteLeaf m)
runRouter routes = toTrie $ execWriter (getRouter routes)
    where
        -- Custom version of Trie.fromList that resolves key conflicts
        -- in the desired manner. In the case of duplicate routes the
        -- routes specified first are favored over any subsequent
        -- specifications.
        toTrie = L.foldl' insertOrReplace Trie.empty
        insertOrReplace t (k, v) =
            let newV = maybe v (mergeValues v) $ Trie.lookup k t
            in Trie.insert k newV t
        mergeValues (Wildcard x) _                              = Wildcard x
        mergeValues _ (Wildcard x)                                     = Wildcard x
        mergeValues RVar RVar                                   = RVar
        mergeValues RVar (RouteMatch x y)                       = RouteMatchOrVar x y
        mergeValues (RouteMatch _ _) (RouteMatch x y)           = RouteMatch x y
        mergeValues (RouteMatch x y) RVar                       = RouteMatchOrVar x y
        mergeValues (RouteMatchOrVar _ _) (RouteMatch x y)      = RouteMatchOrVar x y
        mergeValues (RouteMatchOrVar x y) _                     = RouteMatchOrVar x y
        mergeValues _ v                                                = v

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


-- Routing trie creation algorithm
-- 1. Store full paths as keys up to first `var`
-- 2. Calculate Base64 encoding of the URL portion preceding the
--    `var` ++ "var" and use that as key for the next part of the
--    route spec.
-- 3. Repeat step 2 for every `var` encountered until the route
 --   is completed and maps to a resource.
(#>) :: MonadWriter [(B.ByteString, (RouteLeaf a))] m
     => Route -> Resource a -> m ()
k #> v = do
    let (key, routes, vars, isWild) = foldl routeFoldFun ("", [], [], False) (getRoute k)
        key' = if BC8.null key then "/"
               else key
        ctor = if isWild
                  then Wildcard (RoutedResource k v)
                  else RouteMatch (RoutedResource k v) vars
    tell $ (key', ctor) : routes
    where
        routeFoldFun (kps, rt, vs, False) (Bound x) =
            (B.concat [kps, "/", encodeUtf8 x], rt, vs, False)
        routeFoldFun (kps, rt, vs, False) (Var x) =
            let partKey = Base64.encode $ B.concat [kps, "var"]
                rt' = (kps, RVar) : rt
            in (partKey, rt', x:vs, False)
        routeFoldFun (kps, rt, vs, False) RestUnbound =
            (kps, rt, vs, True)
        routeFoldFun (kps, rt, vs, True) _ =
            (kps, rt, vs, True)


(#>=) :: MonadWriter [(B.ByteString, (RouteLeaf a))] m
      => Route -> m (Resource a) -> m ()
k #>= mv = mv >>= (k #>)


-- | Represents a fully-specified set of routes that map paths (represented as 'Route's) to 'Resource's. 'RoutingSpec's are declared with do-notation, to wit:
--
-- @
--    myRoutes :: RoutingSpec IO ()
--    myRoutes = do
--      root                                 #> myRootResource
--      "blog" '</>' var "date" '</>' var "post" #> blogPostResource
--      "about"                              #> aboutResource
--      "anything" '</>' star                  #> wildcardResource
-- @
--
newtype RoutingSpec m a = RoutingSpec {
        getRouter :: Writer [(B.ByteString, RouteLeaf m)] a
    } deriving ( Functor, Applicative, Monad
               , MonadWriter [(B.ByteString, RouteLeaf m)]
               )


route :: Trie (RouteLeaf a)
      -> BC8.ByteString
      -> Maybe (RoutedResource a, (HashMap Text Text, [Text]))
route routes pInfo = let matchRes = Trie.match routes pInfo
                     in matchRoute' routes matchRes mempty Nothing


matchRoute' :: Trie (RouteLeaf a)
            -> Maybe (B.ByteString, RouteLeaf a, B.ByteString)
            -> [Text]
            -> Maybe B.ByteString
            -> Maybe (RoutedResource a, (HashMap Text Text, [Text]))
matchRoute' _routes Nothing _ps _dsp =
    -- Nothing even partially matched the route
    Nothing
matchRoute' routes (Just (matched, RouteMatchOrVar r vars, "")) ps dsp =
    -- The matched key is also a prefix of other routes, but the
    -- entire path matched so handle like a RouteMatch.
    matchRoute' routes (Just (matched, RouteMatch r vars, "")) ps dsp
matchRoute' _routes (Just (matched, RouteMatch r vars, "")) ps dsp =
    -- The entire path matched so return the resource, params, and
    -- dispatch path
    Just (r, (fromList $ zip vars ps, dispatchList dsp matched))
    where
        dispatchList (Just d) m = toTextList $ B.concat [d, m]
        dispatchList Nothing _ = mempty
        toTextList bs = decodeUtf8 <$> BC8.split '/' bs
matchRoute' _routes (Just (_matched, RouteMatch _r _vars, _)) _ps _dsp =
    -- Part of the request path matched, but the trie value at the
    -- matched prefix is not an RVar or RouteMatchOrVar so there is no
    -- match.
    Nothing
matchRoute' routes (Just (matched, RouteMatchOrVar _r _vars, rest)) ps dsp =
    -- Part of the request path matched and the trie value at the
    -- matched prefix is a RouteMatchOrVar so handle it the same as if
    -- the value were RVar.
    matchRoute' routes (Just (matched, RVar, rest)) ps dsp
matchRoute' routes (Just (matched, RVar, rest)) ps dsp
    | BC8.null rest = Nothing
    | BC8.take 2 rest == "//" = Nothing
    | BC8.head rest == '/' =
        -- Part of the request path matched and the trie value at the
        -- matched prefix is a RVar so calculate the key for the next part
        -- of the route and continue attempting to match.
        let nextKey = B.concat [ Base64.encode $ B.concat [matched, "var"]
                               , BC8.dropWhile (/='/') $ BC8.dropWhile (=='/') rest
                               ]
            updDsp = if isNothing dsp then Just mempty
                     else dsp
            paramVal = decodeUtf8 . BC8.takeWhile (/='/')
                       $ BC8.dropWhile (=='/') rest
            matchRes = Trie.match routes nextKey
        in matchRoute' routes matchRes (paramVal:ps) updDsp
    | otherwise = Nothing
matchRoute' _routes (Just (_matched, Wildcard r, rest)) _ps _dsp =
    -- Encountered a wildcard (star) value in the trie so it's a match
    Just (r, (mempty, decodeUtf8 <$> [BC8.dropWhile (=='/') rest]))
