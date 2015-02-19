{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Decision
    ( flow
    ) where

import           Airship.Date (parseRfc1123Date)
import           Airship.Headers (addResponseHeader)
import           Airship.Types (ContentType, Webmachine, Response(..),
                                ResponseBody(..), halt, request, requestTime,
                                getResponseBody, getResponseHeaders,
                                putResponseBody)
import           Airship.Resource(Resource(..), PostResponse(..))
import           Airship.Parsers (parseEtagList)
import           Control.Applicative ((<$>))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT,
                                                   modify)

import           Data.ByteString (ByteString)
import           Blaze.ByteString.Builder (toByteString)
import qualified Data.CaseInsensitive as CI
import           Data.Maybe (fromJust, isJust)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (requestMethod, requestHeaders, pathInfo)

------------------------------------------------------------------------------
-- HTTP Headers
-- These are headers not defined for us already in
-- Network.HTTP.Types
------------------------------------------------------------------------------

hAcceptCharset :: HTTP.HeaderName
hAcceptCharset = "Accept-Charset"

hAcceptEncoding :: HTTP.HeaderName
hAcceptEncoding = "Accept-Encoding"

hIfMatch :: HTTP.HeaderName
hIfMatch = "If-Match"

hIfUnmodifiedSince :: HTTP.HeaderName
hIfUnmodifiedSince = "If-Unmodified-Since"

hIfNoneMatch :: HTTP.HeaderName
hIfNoneMatch = "If-None-Match"

hIfModifiedSince :: HTTP.HeaderName
hIfModifiedSince = "If-Modified-Since"

------------------------------------------------------------------------------
-- FlowState: StateT used for recording information as we walk the decision
-- tree
------------------------------------------------------------------------------

data FlowState m = FlowState
    { _contentType :: Maybe (ContentType, ResponseBody m)
    }

type FlowStateT s m a = StateT (FlowState m) (Webmachine s m) a

type Flow s m = Resource s m -> FlowStateT s m (Response m)

initFlowState :: FlowState m
initFlowState = FlowState Nothing

flow :: Monad m => Resource s m -> Webmachine s m (Response m)
flow r = evalStateT (b13 r) initFlowState

------------------------------------------------------------------------------
-- Decision Helpers
------------------------------------------------------------------------------

negotiateContentTypesAccepted :: Monad m => Webmachine s m ()
negotiateContentTypesAccepted = undefined

appendRequestPath :: Monad m => [Text] -> Webmachine s m ByteString
appendRequestPath ts = do
    currentPath <- pathInfo <$> request
    return $ toByteString (HTTP.encodePathSegments (currentPath ++ ts))

requestHeaderDate :: Monad m => HTTP.HeaderName ->
                                Webmachine s m (Maybe UTCTime)
requestHeaderDate headerName = do
    req <- request
    let reqHeaders = requestHeaders req
        dateHeader = lookup headerName reqHeaders
        parsedDate = dateHeader >>= parseRfc1123Date
    return parsedDate

------------------------------------------------------------------------------
-- Type definitions for all decision nodes
------------------------------------------------------------------------------

b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03 :: Monad m => Flow s m
c04, c03 :: Monad m => Flow s m
d05, d04 :: Monad m => Flow s m
e06, e05 :: Monad m => Flow s m
f07, f06 :: Monad m => Flow s m
g11, g09, g08, g07 :: Monad m => Flow s m
h12, h11, h10, h07 :: Monad m => Flow s m
i13, i12, i07, i04 :: Monad m => Flow s m
j18 :: Monad m => Flow s m
k13, k07, k05 :: Monad m => Flow s m
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
            let body = fromJust (lookup requestedType provided)
            modify (\fs -> fs { _contentType =
                                    Just (requestedType, body) })
            lift $ putResponseBody body
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
            e05 r

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

f07 r@Resource{..} =
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
        -- TODO: should we be stripping whitespace here?
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

h12 r@Resource{..} = do
    modified <- lift lastModified
    parsedDate <- lift $ requestHeaderDate hIfUnmodifiedSince
    let maybeGreater = do
            lastM <- modified
            headerDate <- parsedDate
            return (lastM > headerDate)
    if maybeGreater == Just True
        then lift $ halt HTTP.status412
        else i12 r

h11 r@Resource{..} = do
    parsedDate <- lift $ requestHeaderDate hIfUnmodifiedSince
    if isJust parsedDate
        then h12 r
        else i12 r

h10 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfUnmodifiedSince reqHeaders of
        (Just _h) ->
            h11 r
        Nothing ->
            i12 r

h07 r@Resource {..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case fromJust (lookup hIfMatch reqHeaders) of
        -- TODO: should we be stripping whitespace here?
        "*" ->
            lift $ halt HTTP.status412
        _ ->
            i07 r

------------------------------------------------------------------------------
-- I column
------------------------------------------------------------------------------

i13 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case fromJust (lookup hIfNoneMatch reqHeaders) of
        -- TODO: should we be stripping whitespace here?
        "*" ->
            j18 r
        _ ->
            k13 r

i12 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfNoneMatch reqHeaders of
        (Just _h) ->
            i13 r
        Nothing ->
            l13 r

i07 r = do
    req <- lift request
    if requestMethod req == HTTP.methodPut
        then i04 r
        else k07 r

i04 r@Resource{..} = do
    moved <- lift movedPermanently
    case moved of
        (Just loc) -> do
            lift $ addResponseHeader ("Location", loc)
            lift $ halt HTTP.status301
        Nothing ->
            p03 r

------------------------------------------------------------------------------
-- J column
------------------------------------------------------------------------------

j18 _ = do
    req <- lift request
    let getOrHead = [ HTTP.methodGet
                    , HTTP.methodHead
                    ]
    if requestMethod req `elem` getOrHead
        then lift $ halt HTTP.status304
        else lift $ halt HTTP.status412

------------------------------------------------------------------------------
-- K column
------------------------------------------------------------------------------

k13 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
        ifNoneMatch = fromJust (lookup hIfNoneMatch reqHeaders)
        etags = parseEtagList ifNoneMatch
    if null etags
        then j18 r
        else l13 r

k07 r@Resource{..} = do
    prevExisted <- lift previouslyExisted
    if prevExisted
        then k05 r
        else l07 r

k05 r@Resource{..} = do
    moved <- lift movedPermanently
    case moved of
        (Just loc) -> do
            lift $ addResponseHeader ("Location", loc)
            lift $ halt HTTP.status301
        Nothing ->
            l05 r

------------------------------------------------------------------------------
-- L column
------------------------------------------------------------------------------

l17 r@Resource{..} = do
    parsedDate <- lift $ requestHeaderDate hIfModifiedSince
    modified <- lift lastModified
    let maybeGreater = do
            lastM <- modified
            ifModifiedSince <- parsedDate
            return (lastM > ifModifiedSince)
    if maybeGreater == Just True
        then m16 r
        else lift $ halt HTTP.status304

l15 r@Resource{..} = do
    parsedDate <- lift $ requestHeaderDate hIfModifiedSince
    now <- lift requestTime
    let maybeGreater = (> now) <$> parsedDate
    if maybeGreater == Just True
        then m16 r
        else l17 r

l14 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
        dateHeader = lookup hIfModifiedSince reqHeaders
        validDate = isJust (dateHeader >>= parseRfc1123Date)
    if validDate
        then l15 r
        else m16 r

l13 r@Resource{..} = do
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfModifiedSince reqHeaders of
        (Just _h) ->
            l14 r
        Nothing ->
            m16 r

l07 r = do
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then m07 r
        else lift $ halt HTTP.status404

l05 r@Resource{..} = do
    moved <- lift movedTemporarily
    case moved of
        (Just loc) -> do
            lift $ addResponseHeader ("Location", loc)
            lift $ halt HTTP.status307
        Nothing ->
            m05 r

------------------------------------------------------------------------------
-- M column
------------------------------------------------------------------------------

m20 r@Resource{..} = do
    deleteAccepted <- lift deleteResource
    if deleteAccepted
        then do
            completed <- lift deleteCompleted
            if completed
                then o20 r
                else lift $ halt HTTP.status202
        else lift $ halt HTTP.status500

m16 r = do
    req <- lift request
    if requestMethod req == HTTP.methodDelete
        then m20 r
        else n16 r

m07 r@Resource{..} = do
    allowMissing <- lift allowMissingPost
    if allowMissing
        then n11 r
        else lift $ halt HTTP.status404

m05 r = do
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then n05 r
        else lift $ halt HTTP.status410

------------------------------------------------------------------------------
-- N column
------------------------------------------------------------------------------

n16 r = do
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then n11 r
        else o16 r

n11 r@Resource{..} = lift processPost >>= flip processPostAction r

create :: Monad m => [Text] -> Webmachine s m ()
create ts = do
    loc <- appendRequestPath ts
    addResponseHeader ("Location", loc)
    negotiateContentTypesAccepted

processPostAction :: Monad m => PostResponse s m -> Flow s m
processPostAction (PostCreate ts) r = do
    lift $ create ts
    p11 r
processPostAction (PostCreateRedirect ts) _r = do
    lift $ create ts
    lift $ halt HTTP.status303
processPostAction (PostProcess p) r =
    lift p >> p11 r
processPostAction (PostProcessRedirect ts) _r = do
    locBs <- lift ts
    lift $ addResponseHeader ("Location", locBs)
    lift $ halt HTTP.status303

n05 r@Resource{..} = do
    allow <- lift allowMissingPost
    if allow
        then n11 r
        else lift $ halt HTTP.status410

------------------------------------------------------------------------------
-- O column
------------------------------------------------------------------------------

o20 r = do
    body <- lift getResponseBody
    -- ResponseBody is a little tough to make an instance of 'Eq',
    -- so we just use a pattern match
    case body of
        Empty   -> lift $ halt HTTP.status204
        _       -> o18 r

o18 Resource{..} = do
    multiple <- lift multipleChoices
    if multiple
        then lift $ halt HTTP.status300
        else do
            -- TODO: set etag, expiration, etc. headers
            provided <- lift contentTypesProvided
            let (cType, body)  = head provided
            lift $ putResponseBody body
            lift $ addResponseHeader ("Content-Type", CI.original cType)
            lift $ halt HTTP.status200

o16 r = do
    req <- lift request
    if requestMethod req == HTTP.methodPut
        then o14 r
        else o18 r

o14 r@Resource{..} = do
    conflict <- lift isConflict
    if conflict
        then lift $ halt HTTP.status409
        else lift negotiateContentTypesAccepted >> p11 r

------------------------------------------------------------------------------
-- P column
------------------------------------------------------------------------------

p11 r = do
    headers <- lift getResponseHeaders
    case lookup HTTP.hLocation headers of
        (Just _) ->
            lift $ halt HTTP.status201
        _ ->
            o20 r

p03 r@Resource{..} = do
    conflict <- lift isConflict
    if conflict
        then lift $ halt HTTP.status409
        else p11 r
