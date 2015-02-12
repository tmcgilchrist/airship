{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Decision
    ( flow
    ) where

import           Airship.Types (Webmachine, Response(..), ResponseBody,
                                halt, request)
import           Airship.Resource(Resource(..), ContentType)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT,
                                                   modify)
import qualified Data.CaseInsensitive as CI
import           Data.Maybe (fromJust)
import qualified Network.HTTP.Types as HTTP
import           Network.Wai (requestMethod, requestHeaders)

data FlowState m = FlowState
    { _contentType :: Maybe (ContentType, ResponseBody m)
    }

type Flow s m = Resource s m -> StateT (FlowState m) (Webmachine s m) (Response m)

initFlowState :: FlowState m
initFlowState = FlowState Nothing

flow :: Monad m => Resource s m -> Webmachine s m (Response m)
flow r = evalStateT (b13 r) initFlowState

b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03 :: Monad m => Flow s m
c04, c03 :: Monad m => Flow s m
d05, d04 :: Monad m => Flow s m

b13 r@Resource{..} = do
    available <- lift serviceAvailable
    if available
        then b12 r
        else lift $ halt HTTP.status503

b12 r@Resource{..} = do
    -- known method
    req <- lift request
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
    if requestMethod req `elem` knownMethods
        then b11 r
        else lift $ halt HTTP.status501

b11 r@Resource{..} = do
    long <- lift uriTooLong
    if long
        then lift $ halt HTTP.status414
        else b10 r

b10 r@Resource{..} = do
    req <- lift request
    allowed <- lift allowedMethods
    if requestMethod req `elem` allowed
        then b09 r
        else lift $ halt HTTP.status405

b09 r@Resource{..} = do
    malformed <- lift malformedRequest
    if malformed
        then lift $ halt HTTP.status400
        else b08 r

b08 r@Resource{..} = do
    authorized <- lift isAuthorized
    if authorized
        then b07 r
        else lift $ halt HTTP.status401

b07 r@Resource{..} = do
    forbid <- lift forbidden
    if forbid
        then lift $ halt HTTP.status403
        else b06 r

b06 r@Resource{..} = do
    validC <- lift validContentHeaders
    if validC
        then b05 r
        else lift $ halt HTTP.status501

b05 r@Resource{..} = do
    known <- lift knownContentType
    if known
        then b04 r
        else lift $ halt HTTP.status415

b04 r@Resource{..} = do
    large <- lift entityTooLarge
    if large
        then lift $ halt HTTP.status413
        else b03 r

b03 r@Resource{..} = do
    req <- lift request
    if requestMethod req == HTTP.methodOptions
        then lift $ halt HTTP.status200
        else c03 r

c04 r@Resource{..} = do
    -- TODO: do proper content-type negotiation
    req <- lift request
    provided <- lift contentTypesProvided

    let types = fmap fst provided
        reqHeaders = requestHeaders req
        -- 'fromJust' should be safe because we should only be called
        -- if an Accept header is present
        requestedType = CI.mk (fromJust (lookup HTTP.hAccept reqHeaders))
    if requestedType `elem` types
        then do
            modify (\fs -> fs { _contentType =
                                    Just (requestedType, undefined) })
            d04 r
        else lift $ halt HTTP.status406

c03 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup HTTP.hAccept reqHeaders of
        (Just _h) ->
            c04 r
        Nothing ->
            d04 r

d04 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup HTTP.hAcceptLanguage reqHeaders of
        (Just _h) ->
            d05 r
        Nothing ->
            -- TODO
            -- e05 r
            undefined

d05 _r@Resource{..} = do
    langAvailable <- lift languageAvailable
    if langAvailable
        then undefined -- e05
        else lift $ halt HTTP.status406
