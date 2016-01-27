{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Airship.Resource2.Decision
  ( flow
  , resource
  ) where

import           Airship.Internal.Decision
import           Airship.Resource2.Data
import           Airship.Types

import           Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Traversable (traverse)

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict

import           Network.HTTP.Media (MediaType)
import qualified Network.HTTP.Types as HTTP


flow :: Monad m => FlowStateT m a -> Webmachine m Response
flow f =
    -- By this point we will always reach a decision point
    flip evalStateT () f >> halt HTTP.status500

resource ::
     Monad m
  => ResourceDesc a b
  -> Webmachine m (Either ResourceDenied (a -> b -> Webmachine m (ResourceExists (FlowStateT m))))
  -> Resource m
resource rd m = do
    (a, (accept, b), re) <- newResource rd m
    lift (re a b) >>= \case
        ResourceExists c run ->
            resourceExists accept c >>= run
        ResourceNotFound run ->
            resourceNotFound accept >>= run

resourceExists :: Monad m => MediaType -> CacheData -> FlowStateT m (ResourceMethod (FlowStateT m))
resourceExists accept cache = do
    _ <- g07 True
    preconditions cache
    let ok'' f = return . f . ((ok' accept cache =<<) .)
    m16 >>= \case
        True -> ok'' RDelete deleteResource
        False -> n16 >>= \case
            True -> ok'' RPost postResource
            False -> o16 >>= \case
                True -> ok'' RPut putResource
                False -> ok'' ROther return

resourceNotFound :: Monad m => MediaType -> FlowStateT m (ResourceNotFoundMethod (FlowStateT m))
resourceNotFound accept = do
    _ <- g07 False
    h07
    i07 >>= \case
        True ->
          return . MPut $ putNewResource accept
        False ->
            -- Peeking ahead to l7/m5 so we can use better types
            fmap ((==) HTTP.methodPost . requestMethod) (lift request) >>= \case
                True -> return . MPost $ postNewResource accept
                False -> return . MOther $ otherNewResource

deleteResource :: Monad m => RDeleteAction -> FlowStateT m ResponseEntity
deleteResource = \case
    RDeleteAccepted -> do
        m20 False
        lift serverError
    RDeleteOk re -> do
        m20 True
        return re

putResource :: Monad m => RPutAction -> FlowStateT m ResponseEntity
putResource = \case
    RPutConflict -> do
        o14 True
        lift serverError
    RPutCreated l -> do
        o14 False
        p11 $ Just l
        lift serverError
    RPutOk re -> do
        o14 False
        p11 Nothing
        return re

postResource :: Monad m => RPostAction -> FlowStateT m ResponseEntity
postResource = \case
    RPostSeeOther l -> do
        n11 $ Just l
        lift serverError
    RPostCreated l -> do
        n11 Nothing
        p11 $ Just l
        lift serverError
    RPostOk re -> do
        n11 Nothing
        p11 Nothing
        return re

postNewResource :: Monad m => MediaType -> MPostAction -> FlowStateT m ()
postNewResource accept = \case
    MPostMoved moved -> do
        _ <- k07 True
        movedResource moved
    MPostGone -> do
        _ <- k07 True
        l07
        k05 Nothing
        l05 Nothing
        m05
        n05 False
    MPostNotFound -> do
        _ <- k07 False
        l07
        m07 False
    MPostSeeOther loc -> do
        _ <- k07 False
        l07
        m07 True
        n11 $ Just loc
    MPostRedirect loc -> do
        _ <- k07 False
        l07
        m07 True
        n11 Nothing
        p11 $ Just loc
    MPostOk cache b -> do
        _ <- k07 False
        l07
        m07 True
        n11 Nothing
        p11 Nothing
        ok' accept cache b

otherNewResource :: Monad m => MOtherAction -> FlowStateT m ()
otherNewResource = \case
    MOtherMoved moved -> do
        _ <- k07 True
        movedResource moved
    MOtherGone -> do
        _ <- k07 True
        k05 Nothing
        l05 Nothing
        m05
        n05 False
        l07
    MOtherNotFound -> do
        _ <- k07 False
        l07

movedResource :: Monad m => Moved -> FlowStateT m ()
movedResource = \case
    MovedPermanently b ->
        k05 $ Just b
    MovedTemporarily b -> do
        k05 Nothing
        l05 $ Just b

putNewResource :: Monad m => MediaType -> MPutAction -> FlowStateT m ()
putNewResource accept = \case
    MPutMoved l ->
        i04 $ Just l
    MPutConflict -> do
        i04 Nothing
        p03 True
    MPutCreated l -> do
        i04 Nothing
        p03 False
        p11 $ Just l
    MPutOk cache re -> do
        i04 Nothing
        p03 False
        p11 Nothing
        ok' accept cache re

ok' :: Monad m => MediaType -> CacheData -> ResponseEntity -> FlowStateT m ()
ok' mt cache (ResponseEntity mr b) = do
    o20 b
    lift $ putResponseBody b
    o18 (mr == MultipleRepresentations) cache mt (return b)

newResource :: Monad m => ResourceDesc a b -> Webmachine m (Either ResourceDenied c) -> FlowStateT m (a, (MediaType, b), c)
newResource (ResourceDesc available methods contentTypes accept lang) rd = do
    b13 $ available == Available
    b12
    b10 methods
    (contentType, c) <- lift rd >>= \case
        Left rd' -> do
            b11 $ rd' == UriTooLong
            b09 $ rd' == MalformedRequest
            b08 $ rd' == Unauthorized
            b07 $ rd' == Forbidden
            b06 $ rd' == UnsupportedContentType
            trace "b05"
            b04 $ rd' == EntityTooLarge
            -- This is because we're not patching matching so that the trace is correct
            lift $ halt HTTP.status500
        Right c -> do
           a <- fmap (fromMaybe (snd $ NE.head contentTypes)) . b05 $ NE.toList contentTypes
           return (a, c)
    b03 methods
    b <- fmap (fromMaybe (NE.head accept)) $ c03 >>= traverse (\a -> c04 a (NE.toList accept))
    d04 >>= traverse_ (d05 . lang)
    e05 >>= traverse_ e06
    f06 >>= traverse_ f07
    return (contentType, b, c)
