{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Internal.Decision
    ( flow
    , appendRequestPath
    ) where

import           Airship.Internal.Date (parseRfc1123Date, utcTimeToRfc1123)
import           Airship.Headers (addResponseHeader)
import           Airship.Types ( Response(..)
                               , ResponseBody(..)
                               , Webmachine
                               , etagToByteString
                               , getResponseBody
                               , getResponseHeaders
                               , halt
                               , pathInfo
                               , putResponseBody
                               , request
                               , requestHeaders
                               , requestMethod
                               , requestTime )

import           Airship.Resource(Resource(..), PostResponse(..))
import           Airship.Internal.Parsers (parseEtagList)
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif
import           Control.Monad (when)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT,
                                                   get, modify)
import           Control.Monad.Writer.Class (tell)

import           Blaze.ByteString.Builder (toByteString)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.ByteString                  (ByteString, intercalate)

import           Network.HTTP.Media
import qualified Network.HTTP.Types as HTTP

------------------------------------------------------------------------------
-- HTTP Headers
-- These are headers not defined for us already in
-- Network.HTTP.Types
------------------------------------------------------------------------------
-- TODO this exist in http-types-0.9, see CHANGES.txt
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

------------------------------------------------------------------------------
-- FlowState: StateT used for recording information as we walk the decision
-- tree
------------------------------------------------------------------------------

data FlowState m = FlowState
    { _contentType :: Maybe (MediaType, Webmachine m ResponseBody) }

type FlowStateT m a = StateT (FlowState m) (Webmachine m) a

type Flow m = Resource m -> FlowStateT m Response

initFlowState :: FlowState m
initFlowState = FlowState Nothing

flow :: Monad m => Resource m -> Webmachine m Response
flow r = evalStateT (b13 r) initFlowState

trace :: Monad m => Text -> FlowStateT m ()
trace t = lift $ tell [t]

-----------------------------------------------------------------------------
-- Header value data newtypes
------------------------------------------------------------------------------

newtype IfMatch = IfMatch ByteString
newtype IfNoneMatch = IfNoneMatch ByteString

------------------------------------------------------------------------------
-- Decision Helpers
------------------------------------------------------------------------------

negotiateContentTypesAccepted :: Monad m => Resource m -> FlowStateT m ()
negotiateContentTypesAccepted Resource{..} = do
    req <- lift request
    accepted <- lift contentTypesAccepted
    let reqHeaders = requestHeaders req
        result = do
            cType <- lookup HTTP.hContentType reqHeaders
            mapContentMedia accepted cType
    case result of
        (Just process) -> lift process
        Nothing -> lift $ halt HTTP.status415

appendRequestPath :: Monad m => [Text] -> Webmachine m ByteString
appendRequestPath ts = do
    currentPath <- pathInfo <$> request
    return $ toByteString (HTTP.encodePathSegments (currentPath ++ ts))

requestHeaderDate :: Monad m => HTTP.HeaderName ->
                                Webmachine m (Maybe UTCTime)
requestHeaderDate headerName = do
    req <- request
    let reqHeaders = requestHeaders req
        dateHeader = lookup headerName reqHeaders
        parsedDate = dateHeader >>= parseRfc1123Date
    return parsedDate

writeCacheTags :: Monad m => Resource m -> FlowStateT m ()
writeCacheTags Resource{..} = lift $ do
    etag <- generateETag
    case etag of
       Nothing -> return ()
       Just t  -> addResponseHeader ("ETag", etagToByteString t)
    modified <- lastModified
    case modified of
       Nothing -> return ()
       Just d  -> addResponseHeader ("Last-Modified", utcTimeToRfc1123 d)

------------------------------------------------------------------------------
-- Type definitions for all decision nodes
------------------------------------------------------------------------------

b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03 :: Monad m => Flow  m
c04, c03 :: Monad m => Flow  m
d05, d04 :: Monad m => Flow  m
e06, e05 :: Monad m => Flow  m
f07, f06 :: Monad m => Flow  m
g11, g09 :: Monad m => IfMatch -> Flow m
g08, g07 :: Monad m => Flow  m
h12, h11, h10, h07 :: Monad m => Flow  m
i13 :: Monad m => IfNoneMatch -> Flow m
i12, i07, i04 :: Monad m => Flow  m
j18 :: Monad m => Flow  m
k13 :: Monad m => IfNoneMatch -> Flow m
k07, k05 :: Monad m => Flow  m
l17, l15, l14, l13, l07, l05 :: Monad m => Flow  m
m20, m16, m07, m05 :: Monad m => Flow  m
n16, n11, n05 :: Monad m => Flow  m
o20, o18, o16, o14 :: Monad m => Flow  m
p11, p03 :: Monad m => Flow  m

------------------------------------------------------------------------------
-- B column
------------------------------------------------------------------------------

b13 r@Resource{..} = do
    trace "b13"
    available <- lift serviceAvailable
    if available
        then b12 r
        else lift $ halt HTTP.status503

b12 r@Resource{..} = do
    trace "b12"
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
    trace "b11"
    long <- lift uriTooLong
    if long
        then lift $ halt HTTP.status414
        else b10 r

b10 r@Resource{..} = do
    trace "b10"
    req <- lift request
    allowed <- lift allowedMethods
    if requestMethod req `elem` allowed
        then b09 r
        else do
            lift $ addResponseHeader ("Allow",  intercalate "," allowed)
            lift $ halt HTTP.status405

b09 r@Resource{..} = do
    trace "b09"
    malformed <- lift malformedRequest
    if malformed
        then lift $ halt HTTP.status400
        else b08 r

b08 r@Resource{..} = do
    trace "b08"
    authorized <- lift isAuthorized
    if authorized
        then b07 r
        else lift $ halt HTTP.status401

b07 r@Resource{..} = do
    trace "b07"
    forbid <- lift forbidden
    if forbid
        then lift $ halt HTTP.status403
        else b06 r

b06 r@Resource{..} = do
    trace "b06"
    validC <- lift validContentHeaders
    if validC
        then b05 r
        else lift $ halt HTTP.status501

b05 r@Resource{..} = do
    trace "b05"
    known <- lift knownContentType
    if known
        then b04 r
        else lift $ halt HTTP.status415

b04 r@Resource{..} = do
    trace "b04"
    large <- lift entityTooLarge
    if large
        then lift $ halt HTTP.status413
        else b03 r

b03 r@Resource{..} = do
    trace "b03"
    req <- lift request
    allowed <- lift allowedMethods
    if requestMethod req == HTTP.methodOptions
        then do
            lift $ addResponseHeader ("Allow",  intercalate "," allowed)
            lift $ halt HTTP.status204
        else c03 r

------------------------------------------------------------------------------
-- C column
------------------------------------------------------------------------------

c04 r@Resource{..} = do
    trace "c04"
    req <- lift request
    provided <- lift contentTypesProvided
    let reqHeaders = requestHeaders req
        result = do
            acceptStr <- lookup HTTP.hAccept reqHeaders
            (acceptTyp, resource) <- mapAcceptMedia provided' acceptStr
            Just (acceptTyp, resource)
            where
                -- this is so that in addition to getting back the resource
                -- that we match, we also return the content-type provided
                -- by that resource.
                provided' = map dupContentType provided
                dupContentType (a, b) = (a, (a, b))

    case result of
      Nothing -> lift $ halt HTTP.status406
      Just res -> do
        modify (\fs -> fs { _contentType = Just res })
        d04 r

c03 r@Resource{..} = do
    trace "c03"
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
    trace "d05"
    langAvailable <- lift languageAvailable
    if langAvailable
        then e05 r
        else lift $ halt HTTP.status406

d04 r@Resource{..} = do
    trace "d04"
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup HTTP.hAcceptLanguage reqHeaders of
        (Just _h) ->
            d05 r
        Nothing ->
            e05 r

------------------------------------------------------------------------------
-- E column
------------------------------------------------------------------------------

e06 r@Resource{..} = do
    trace "e06"
    -- TODO: charset negotiation
    f06 r

e05 r@Resource{..} = do
    trace "e05"
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
    trace "f07"
    -- TODO: encoding negotiation
    g07 r

f06 r@Resource{..} = do
    trace "f06"
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

g11 (IfMatch ifMatch) r@Resource{..} = do
    trace "g11"
    let etags = parseEtagList ifMatch
    if null etags
        then lift $ halt HTTP.status412
        else h10 r

g09 ifMatch r@Resource{..} = do
    trace "g09"
    case ifMatch of
        -- TODO: should we be stripping whitespace here?
        (IfMatch "*") ->
            h10 r
        _ ->
            g11 ifMatch r

g08 r@Resource{..} = do
    trace "g08"
    req <- lift request
    let reqHeaders = requestHeaders req
    case IfMatch <$> lookup hIfMatch reqHeaders of
        (Just h) ->
            g09 h r
        Nothing ->
            h10 r

g07 r@Resource{..} = do
    trace "g07"
    -- TODO: set Vary headers
    exists <- lift resourceExists
    if exists
        then g08 r
        else h07 r

------------------------------------------------------------------------------
-- H column
------------------------------------------------------------------------------

h12 r@Resource{..} = do
    trace "h12"
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
    trace "h11"
    parsedDate <- lift $ requestHeaderDate hIfUnmodifiedSince
    if isJust parsedDate
        then h12 r
        else i12 r

h10 r@Resource{..} = do
    trace "h10"
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfUnmodifiedSince reqHeaders of
        (Just _h) ->
            h11 r
        Nothing ->
            i12 r

h07 r@Resource {..} = do
    trace "h07"
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfMatch reqHeaders of
        -- TODO: should we be stripping whitespace here?
        (Just "*") ->
            lift $ halt HTTP.status412
        _ ->
            i07 r

------------------------------------------------------------------------------
-- I column
------------------------------------------------------------------------------

i13 ifNoneMatch r@Resource{..} = do
    trace "i13"
    case ifNoneMatch of
        -- TODO: should we be stripping whitespace here?
        (IfNoneMatch "*") ->
            j18 r
        _ ->
            k13 ifNoneMatch r

i12 r@Resource{..} = do
    trace "i12"
    req <- lift request
    let reqHeaders = requestHeaders req
    case IfNoneMatch <$> lookup hIfNoneMatch reqHeaders of
        (Just h) ->
            i13 h r
        Nothing ->
            l13 r

i07 r = do
    trace "i07"
    req <- lift request
    if requestMethod req == HTTP.methodPut
        then i04 r
        else k07 r

i04 r@Resource{..} = do
    trace "i04"
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
    trace "j18"
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

k13 (IfNoneMatch ifNoneMatch) r@Resource{..} = do
    trace "k13"
    let etags = parseEtagList ifNoneMatch
    if null etags
        then l13 r
        else j18 r

k07 r@Resource{..} = do
    trace "k07"
    prevExisted <- lift previouslyExisted
    if prevExisted
        then k05 r
        else l07 r

k05 r@Resource{..} = do
    trace "k05"
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
    trace "l17"
    parsedDate <- lift $ requestHeaderDate HTTP.hIfModifiedSince
    modified <- lift lastModified
    let maybeGreater = do
            lastM <- modified
            ifModifiedSince <- parsedDate
            return (lastM > ifModifiedSince)
    if maybeGreater == Just True
        then m16 r
        else lift $ halt HTTP.status304

l15 r@Resource{..} = do
    trace "l15"
    parsedDate <- lift $ requestHeaderDate HTTP.hIfModifiedSince
    now <- lift requestTime
    let maybeGreater = (> now) <$> parsedDate
    if maybeGreater == Just True
        then m16 r
        else l17 r

l14 r@Resource{..} = do
    trace "l14"
    req <- lift request
    let reqHeaders = requestHeaders req
        dateHeader = lookup HTTP.hIfModifiedSince reqHeaders
        validDate = isJust (dateHeader >>= parseRfc1123Date)
    if validDate
        then l15 r
        else m16 r

l13 r@Resource{..} = do
    trace "l13"
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup HTTP.hIfModifiedSince reqHeaders of
        (Just _h) ->
            l14 r
        Nothing ->
            m16 r

l07 r = do
    trace "l07"
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then m07 r
        else lift $ halt HTTP.status404

l05 r@Resource{..} = do
    trace "l05"
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
    trace "m20"
    deleteAccepted <- lift deleteResource
    if deleteAccepted
        then do
            completed <- lift deleteCompleted
            if completed
                then o20 r
                else lift $ halt HTTP.status202
        else lift $ halt HTTP.status500

m16 r = do
    trace "m16"
    req <- lift request
    if requestMethod req == HTTP.methodDelete
        then m20 r
        else n16 r

m07 r@Resource{..} = do
    trace "m07"
    allowMissing <- lift allowMissingPost
    if allowMissing
        then n11 r
        else lift $ halt HTTP.status404

m05 r = do
    trace "m05"
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then n05 r
        else lift $ halt HTTP.status410

------------------------------------------------------------------------------
-- N column
------------------------------------------------------------------------------

n16 r = do
    trace "n16"
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then n11 r
        else o16 r

n11 r@Resource{..} = trace "n11" >> lift processPost >>= flip processPostAction r

create :: Monad m => [Text] -> Resource m -> FlowStateT m ()
create ts r = do
    loc <- lift (appendRequestPath ts)
    lift (addResponseHeader ("Location", loc))
    negotiateContentTypesAccepted r

processPostAction :: Monad m => PostResponse m -> Flow  m
processPostAction (PostCreate ts) r = do
    create ts r
    p11 r
processPostAction (PostCreateRedirect ts) r = do
    create ts r
    lift $ halt HTTP.status303
processPostAction (PostProcess p) r =
    lift p >> p11 r
processPostAction (PostProcessRedirect ts) _r = do
    locBs <- lift ts
    lift $ addResponseHeader ("Location", locBs)
    lift $ halt HTTP.status303

n05 r@Resource{..} = do
    trace "n05"
    allow <- lift allowMissingPost
    if allow
        then n11 r
        else lift $ halt HTTP.status410

------------------------------------------------------------------------------
-- O column
------------------------------------------------------------------------------

o20 r = do
    trace "o20"
    body <- lift getResponseBody
    -- ResponseBody is a little tough to make an instance of 'Eq',
    -- so we just use a pattern match
    case body of
        Empty   -> lift $ halt HTTP.status204
        _       -> o18 r

o18 r@Resource{..} = do
    trace "o18"
    multiple <- lift multipleChoices
    if multiple
        then lift $ halt HTTP.status300
        else do
            -- TODO: set etag, expiration, etc. headers
            req <- lift request
            let getOrHead = [ HTTP.methodGet
                            , HTTP.methodHead
                            ]
            when (requestMethod req `elem` getOrHead) $ do
                m <- _contentType <$> get
                (cType, body) <- case m of
                    Nothing -> do
                        provided <- lift contentTypesProvided
                        return (head provided)
                    Just (cType, body) ->
                        return (cType, body)
                b <- lift body
                lift $ putResponseBody b
                lift $ addResponseHeader ("Content-Type", renderHeader cType)
            writeCacheTags r
            lift $ halt HTTP.status200

o16 r = do
    trace "o16"
    req <- lift request
    if requestMethod req == HTTP.methodPut
        then o14 r
        else o18 r

o14 r@Resource{..} = do
    trace "o14"
    conflict <- lift isConflict
    if conflict
        then lift $ halt HTTP.status409
        else negotiateContentTypesAccepted r >> p11 r

------------------------------------------------------------------------------
-- P column
------------------------------------------------------------------------------

p11 r = do
    trace "p11"
    headers <- lift getResponseHeaders
    case lookup HTTP.hLocation headers of
        (Just _) ->
            lift $ halt HTTP.status201
        _ ->
            o20 r

p03 r@Resource{..} = do
    trace "p03"
    conflict <- lift isConflict
    if conflict
        then lift $ halt HTTP.status409
        else negotiateContentTypesAccepted r >> p11 r
