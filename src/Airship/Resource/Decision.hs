{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Airship.Resource.Decision
  ( flow
  ) where

import           Airship.Headers (addResponseHeader)
import           Airship.Internal.Decision
import           Airship.Resource
import           Airship.Types

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>), (<*>))
#endif
import           Control.Monad (unless)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State.Strict (evalStateT)

import           Data.ByteString (ByteString)
import           Data.Foldable (traverse_)
import           Data.Traversable (traverse)

import           Network.HTTP.Media (MediaType)
import qualified Network.HTTP.Types as HTTP


flow :: Monad m => Resource m -> Webmachine m Response
flow r@Resource{..} = flip evalStateT () $ do
    cta <- validate r
    ctp <- accept r
    cache <- lift resourceExists >>= g07 >>= \case
        False -> do
            create r cta
            lift $ resourceCacheData r
        True -> do
            cache <- lift $ resourceCacheData r
            preconditions cache
            process r cta
            return cache
    ok r ctp cache

validate :: Monad m => Resource m -> FlowStateT m (Maybe (Webmachine m ()))
validate Resource{..} = do
    lift serviceAvailable >>= b13
    b12
    lift uriTooLong >>= b11
    lift allowedMethods >>= b10
    lift malformedRequest >>= b09
    lift isAuthorized >>= b08
    lift forbidden >>= b07
    lift validContentHeaders >>= b06
    ct <- lift contentTypesAccepted >>= b05
    lift entityTooLarge >>= b04
    lift allowedMethods >>= b03
    return ct

accept :: Monad m => Resource m -> FlowStateT m (Maybe (MediaType, Webmachine m ResponseBody))
accept Resource{..} = do
    ctp <- c03 >>= traverse (\a -> lift contentTypesProvided >>= c04 a)
    d04 >>= traverse_ (\_ -> lift languageAvailable >>= d05)
    e05 >>= traverse_ e06
    f06 >>= traverse_ f07
    return ctp

create :: Monad m => Resource m -> Maybe (Webmachine m ()) -> FlowStateT m ()
create r@Resource{..} cta = do
    h07
    i07 >>= \case
        True -> do
            lift movedPermanently >>= i04 . fmap Location
            lift isConflict >>= p03
            traverse_ lift cta
            p11' r
        False ->
            lift previouslyExisted >>= k07 >>= \case
                True -> do
                    lift movedPermanently >>= k05 . fmap Location
                    lift movedTemporarily >>= l05 . fmap Location
                    m05
                    lift allowMissingPost >>= n05
                    n11' r
                False -> do
                    l07
                    lift allowMissingPost >>= m07
                    n11' r

process :: Monad m => Resource m -> Maybe (Webmachine m ()) -> FlowStateT m ()
process r@Resource{..} cta =
    m16 >>= \case
        True -> do
            lift deleteResource >>= flip unless (lift $ halt HTTP.status500)
            lift deleteCompleted >>= m20
            lift getResponseBody >>= o20
        False ->
            n16 >>= \case
               True ->
                   n11' r
               False ->
                   o16 >>= \case
                       False ->
                           return ()
                       True -> do
                           lift isConflict >>= o14
                           traverse_ lift cta
                           p11' r

ok :: Monad m => Resource m -> Maybe (MediaType, Webmachine m ResponseBody) -> CacheData -> FlowStateT m Response
ok Resource{..} ctp cache = do
    mc <- lift multipleChoices
    (a, p) <- maybe (head <$> lift contentTypesProvided) return ctp
    o18 mc cache a p

n11' :: Monad m => Resource m -> FlowStateT m ()
n11' Resource{..} = do
    lift processPost >>= processPostAction
    lift responseLocation >>= p11 . fmap Location
    lift getResponseBody >>= o20

p11' :: Monad m => Resource m -> FlowStateT m ()
p11' Resource{..} = do
    lift responseLocation >>= p11 . fmap Location
    lift getResponseBody >>= o20

processPostAction :: Monad m => PostResponse m -> FlowStateT m ()
processPostAction = \case
    PostCreate ts -> do
        n11 Nothing
        loc <- lift (appendRequestPath ts)
        lift (addResponseHeader ("Location", loc))
    PostProcess p -> do
        n11 Nothing
        lift p
    PostProcessRedirect ts -> do
        locBs <- lift ts
        n11 . Just $ Location locBs

resourceCacheData :: Monad m => Resource m -> Webmachine m CacheData
resourceCacheData Resource{..} =
    CacheData <$> lastModified <*> generateETag

responseLocation :: Monad m => Webmachine m (Maybe ByteString)
responseLocation =
    lookup HTTP.hLocation <$> getResponseHeaders
