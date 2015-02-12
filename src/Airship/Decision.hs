{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Decision
    ( flow
    ) where

import           Airship.Types (Handler, Response(..), halt, request)
import           Airship.Resource(Resource(..))
import qualified Data.CaseInsensitive as CI
import           Data.Maybe (fromJust)
import qualified Network.HTTP.Types as HTTP
import           Network.Wai (requestMethod, requestHeaders)

type Flow s m = Resource s m -> Handler s m (Response m)

flow :: Flow s m
flow = b13

b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03 :: Flow s m
c04, c03 :: Flow s m
d05, d04 :: Flow s m

b13 r@Resource{..} = do
    available <- serviceAvailable
    if available
        then b12 r
        else halt HTTP.status503

b12 r@Resource{..} = do
    -- known method
    req <- request
    let knownMethods = [ HTTP.methodGet
                       , HTTP.methodPost
                       , HTTP.methodHead
                       , HTTP.methodPut
                       , HTTP.methodDelete
                       , HTTP.methodTrace
                       , HTTP.methodConnect
                       , HTTP.methodOptions
                       , HTTP.methodPatch
                       ]
    if (requestMethod req) `elem` knownMethods
        then b11 r
        else halt HTTP.status501

b11 r@Resource{..} = do
    long <- uriTooLong
    if long
        then halt HTTP.status414
        else b10 r

b10 r@Resource{..} = do
    req <- request
    allowed <- allowedMethods
    if (requestMethod req) `elem` allowed
        then b09 r
        else halt HTTP.status405

b09 r@Resource{..} = do
    malformed <- malformedRequest
    if malformed
        then halt HTTP.status400
        else b08 r

b08 r@Resource{..} = do
    authorized <- isAuthorized
    if authorized
        then b07 r
        else halt HTTP.status401

b07 r@Resource{..} = do
    forbid <- forbidden
    if forbid
        then halt HTTP.status403
        else b06 r

b06 r@Resource{..} = do
    validC <- validContentHeaders
    if validC
        then b05 r
        else halt HTTP.status501

b05 r@Resource{..} = do
    known <- knownContentType
    if known
        then b04 r
        else halt HTTP.status415

b04 r@Resource{..} = do
    large <- entityTooLarge
    if large
        then halt HTTP.status413
        else b03 r

b03 r@Resource{..} = do
    req <- request
    if (requestMethod req) == HTTP.methodOptions
        then halt HTTP.status200
        else c03 r

c04 r@Resource{..} = do
    -- TODO: do proper content-type negotiation
    req <- request
    provided <- contentTypesProvided

    let types = fmap fst provided
        reqHeaders = requestHeaders req
        -- 'fromJust' should be safe because we should only be called
        -- if an Accept header is present
        requestedType = CI.mk (fromJust (lookup HTTP.hAccept reqHeaders))
    if requestedType `elem` types
        then d04 r
        else halt HTTP.status406

c03 r@Resource{..} = do
    req <- request
    let reqHeaders = requestHeaders req
    case lookup HTTP.hAccept reqHeaders of
        (Just _h) ->
            c04 r
        Nothing ->
            d04 r

d04 = undefined
d05 = undefined
