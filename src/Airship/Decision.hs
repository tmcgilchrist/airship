{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Decision
    ( flow
    ) where

import           Airship.Types (ContentType, Webmachine, Response(..),
                                ResponseBody, halt, request)
import           Airship.Resource(Resource(..))
import           Airship.Parsers (parseEtagList)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT,
                                                   modify)
import qualified Data.CaseInsensitive as CI
import           Data.Maybe (fromJust)
import qualified Network.HTTP.Types as HTTP
import           Network.Wai (requestMethod, requestHeaders)

hAcceptCharset :: HTTP.HeaderName
hAcceptCharset = "Accept-Charset"

hAcceptEncoding :: HTTP.HeaderName
hAcceptEncoding = "Accept-Encoding"

hIfMatch :: HTTP.HeaderName
hIfMatch = "If-Match"

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
e06, e05 :: Monad m => Flow s m
f07, f06 :: Monad m => Flow s m
g11, g09, g08, g07 :: Monad m => Flow s m
h12, h11, h10, h07 :: Monad m => Flow s m
i13, i12, i07, i04 :: Monad m => Flow s m
j18 :: Monad m => Flow s m
k07, k05 :: Monad m => Flow s m
l17, l15, l14, l13, l07, l05 :: Monad m => Flow s m
m20, m16, m07, m05 :: Monad m => Flow s m
n16, n11, n05 :: Monad m => Flow s m
o20, o18, o16, o14 :: Monad m => Flow s m
p11, p03 :: Monad m => Flow s m

------------------------------------------------------------------------------
-- B column
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- C column
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- D column
------------------------------------------------------------------------------

d05 r@Resource{..} = do
    langAvailable <- lift languageAvailable
    if langAvailable
        then e05 r
        else lift $ halt HTTP.status406

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

------------------------------------------------------------------------------
-- D column
------------------------------------------------------------------------------

e06 r@Resource{..} =
    -- TODO: charset negotiation
    f06 r

e05 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hAcceptCharset reqHeaders of
        (Just _h) ->
            e06 r
        Nothing ->
            f06 r

------------------------------------------------------------------------------
-- F column
------------------------------------------------------------------------------

f07 r@Resource{..} = do
   -- TODO: encoding negotiation
   g07 r

f06 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hAcceptEncoding reqHeaders of
        (Just _h) ->
            f07 r
        Nothing ->
            g07 r

------------------------------------------------------------------------------
-- G column
------------------------------------------------------------------------------

g11 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
        ifMatch = fromJust (lookup hIfMatch reqHeaders)
        etags = parseEtagList ifMatch
    if null etags
        then lift $ halt HTTP.status412
        else h10 r

g09 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case fromJust (lookup hIfMatch reqHeaders) of
        -- TODO: should we be stripped whitespace here?
        "*" ->
            h10 r
        _ ->
            g11 r

g08 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfMatch reqHeaders of
        (Just _h) ->
            g09 r
        Nothing ->
            h10 r

g07 r@Resource{..} = do
    -- TODO: set Vary headers
    exists <- lift resourceExists
    if exists
        then g08 r
        else h07 r

------------------------------------------------------------------------------
-- H column
------------------------------------------------------------------------------

h12 = undefined
h11 = undefined
h10 = undefined
h07 = undefined

------------------------------------------------------------------------------
-- I column
------------------------------------------------------------------------------

i13 = undefined
i12 = undefined
i07 = undefined
i04 = undefined

------------------------------------------------------------------------------
-- J column
------------------------------------------------------------------------------

j18 = undefined

------------------------------------------------------------------------------
-- K column
------------------------------------------------------------------------------

k07 = undefined
k05 = undefined

------------------------------------------------------------------------------
-- L column
------------------------------------------------------------------------------

l17 = undefined
l15 = undefined
l14 = undefined
l13 = undefined
l07 = undefined
l05 = undefined

------------------------------------------------------------------------------
-- M column
------------------------------------------------------------------------------

m20 = undefined
m16 = undefined
m07 = undefined
m05 = undefined

------------------------------------------------------------------------------
-- N column
------------------------------------------------------------------------------

n16 = undefined
n11 = undefined
n05 = undefined

------------------------------------------------------------------------------
-- O column
------------------------------------------------------------------------------

o20 = undefined
o18 = undefined
o16 = undefined
o14 = undefined

------------------------------------------------------------------------------
-- P column
------------------------------------------------------------------------------

p11 = undefined
p03 = undefined
