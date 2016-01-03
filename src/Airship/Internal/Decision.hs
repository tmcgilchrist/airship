{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Internal.Decision where

import           Airship.Internal.Date (parseRfc1123Date, utcTimeToRfc1123)
import           Airship.Headers (addResponseHeader, setResponseHeader)
import           Airship.Types ( ETag(..)
                               , CacheData(..)
                               , Location(..)
                               , ResponseBody(..)
                               , Webmachine
                               , etagToByteString
                               , halt
                               , pathInfo
                               , putResponseBody
                               , request
                               , requestHeaders
                               , requestMethod
                               , requestTime )

import           Airship.Internal.Parsers (parseEtagList)
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif
import           Control.Monad ((>=>), unless, when)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Control.Monad.Writer.Class (tell)

import           Blaze.ByteString.Builder (toByteString)
import           Data.Foldable (traverse_)
import qualified Data.Foldable as F
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

type FlowStateT m = StateT () (Webmachine m)


trace :: Monad m => Text -> FlowStateT m ()
trace t = lift $ tell [t]

-----------------------------------------------------------------------------
-- Header value data newtypes
------------------------------------------------------------------------------

newtype IfMatch = IfMatch ByteString
newtype IfNoneMatch = IfNoneMatch ByteString
newtype IfModifiedSinceHeader = IfModifiedSinceHeader ByteString
newtype IfModifiedSince = IfModifiedSince UTCTime
newtype IfUnmodifiedSinceHeader = IfUnmodifiedSinceHeader ByteString
newtype IfUnmodifiedSince = IfUnmodifiedSince UTCTime

newtype AcceptHeader = AcceptHeader ByteString
newtype AcceptLanguage = AcceptLanguage ByteString
newtype AcceptCharset = AcceptCharset ByteString
newtype AcceptEncoding = AcceptEncoding ByteString

------------------------------------------------------------------------------
-- Decision Helpers
------------------------------------------------------------------------------

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

preconditions :: Monad m => CacheData -> FlowStateT m ()
preconditions (CacheData modified' etag) = do
    ifMatch' etag
    ifUnmodifiedSince' modified'
    ifNoneMatch' etag
    ifModifiedSince' modified'

ifMatch' :: Monad m => Maybe ETag -> FlowStateT m ()
ifMatch' etag =
    g08 >>= traverse_ (g09 >=> traverse_ (g11 etag))

ifUnmodifiedSince' :: Monad m => Maybe UTCTime -> FlowStateT m ()
ifUnmodifiedSince' lastModified =
    h10 >>= traverse_ (h11 >=> traverse_ (h12 lastModified))

ifNoneMatch' :: Monad m => Maybe ETag -> FlowStateT m ()
ifNoneMatch' etag =
    i12 >>= traverse_ (i13 >=> maybe j18 (k13 etag >=> flip unless j18))

ifModifiedSince' :: Monad m => Maybe UTCTime -> FlowStateT m ()
ifModifiedSince' lastModified = do
    l13 >>= traverse_ (l14 >=> traverse_ (\ims -> l15 ims >>= flip unless (l17 lastModified ims)))

------------------------------------------------------------------------------
-- B column
------------------------------------------------------------------------------

b13 :: Monad m => Bool -> FlowStateT m ()
b13 available = do
    trace "b13"
    if available
        then return ()
        else lift $ halt HTTP.status503

b12 :: Monad m => FlowStateT m ()
b12 = do
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
        then return ()
        else lift $ halt HTTP.status501

b11 :: Monad m => Bool -> FlowStateT m ()
b11 long = do
    trace "b11"
    if long
        then lift $ halt HTTP.status414
        else return ()

b10 :: Monad m => [HTTP.Method] -> FlowStateT m ()
b10 allowed = do
    trace "b10"
    req <- lift request
    if requestMethod req `elem` allowed
        then return ()
        else do
            lift $ addResponseHeader ("Allow",  intercalate "," allowed)
            lift $ halt HTTP.status405

b09 :: Monad m => Bool -> FlowStateT m ()
b09 malformed = do
    trace "b09"
    if malformed
        then lift $ halt HTTP.status400
        else return ()

b08 :: Monad m => Bool -> FlowStateT m ()
b08 authorized = do
    trace "b08"
    if authorized
        then return ()
        else lift $ halt HTTP.status401

b07 :: Monad m => Bool -> FlowStateT m ()
b07 forbid = do
    trace "b07"
    if forbid
        then lift $ halt HTTP.status403
        else return ()

b06 :: Monad m => Bool -> FlowStateT m ()
b06 validC = do
    trace "b06"
    if validC
        then return ()
        else lift $ halt HTTP.status501

b05 :: Monad m => Bool -> FlowStateT m ()
b05 known = do
    trace "b05"
    if known
        then return ()
        else lift $ halt HTTP.status415

b04 :: Monad m => Bool-> FlowStateT m ()
b04 large = do
    trace "b04"
    if large
        then lift $ halt HTTP.status413
        else return ()

b03 :: Monad m => [HTTP.Method] -> FlowStateT m ()
b03 allowed = do
    trace "b03"
    req <- lift request
    if requestMethod req == HTTP.methodOptions
        then do
            lift $ addResponseHeader ("Allow",  intercalate "," allowed)
            lift $ halt HTTP.status204
        else return ()

------------------------------------------------------------------------------
-- C column
------------------------------------------------------------------------------

c04 :: Monad m => AcceptHeader -> [(MediaType, a)] -> FlowStateT m (MediaType, a)
c04 (AcceptHeader acceptStr) provided = do
    trace "c04"
    let result = do
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
        Just res ->
            return res

c03 :: Monad m => FlowStateT m (Maybe AcceptHeader)
c03 = do
    trace "c03"
    fmap AcceptHeader . lookup HTTP.hAccept . requestHeaders <$> lift request

------------------------------------------------------------------------------
-- D column
------------------------------------------------------------------------------

d05 :: Monad m => Bool -> FlowStateT m ()
d05 langAvailable = do
    trace "d05"
    if langAvailable
        then return ()
        else lift $ halt HTTP.status406

d04 :: Monad m => FlowStateT m (Maybe AcceptLanguage)
d04 = do
    trace "d04"
    fmap AcceptLanguage . lookup HTTP.hAcceptLanguage . requestHeaders <$> lift request

------------------------------------------------------------------------------
-- E column
------------------------------------------------------------------------------

e06 :: Monad m => AcceptCharset -> FlowStateT m ()
e06 (AcceptCharset _) = do
    trace "e06"
    -- TODO: charset negotiation
    return ()

e05 :: Monad m => FlowStateT m (Maybe AcceptCharset)
e05 = do
    trace "e05"
    fmap AcceptCharset . lookup hAcceptCharset . requestHeaders <$> lift request


------------------------------------------------------------------------------
-- F column
------------------------------------------------------------------------------

f07 :: Monad m => AcceptEncoding -> FlowStateT m ()
f07 (AcceptEncoding _) = do
    trace "f07"
    -- TODO: encoding negotiation
    return ()

f06 :: Monad m => FlowStateT m (Maybe AcceptEncoding)
f06 = do
    trace "f06"
    req <- lift request
    return . fmap AcceptEncoding . lookup hAcceptEncoding . requestHeaders $ req

------------------------------------------------------------------------------
-- G column
------------------------------------------------------------------------------

g11 :: Monad m => Maybe ETag -> IfMatch -> FlowStateT m ()
g11 etag (IfMatch ifMatch) = do
    trace "g11"
    let etags = parseEtagList ifMatch
    if any (flip F.elem etag) etags
        then lift $ halt HTTP.status412
        else return ()

g09 :: Monad m => IfMatch -> FlowStateT m (Maybe IfMatch)
g09 ifMatch = do
    trace "g09"
    case ifMatch of
        -- TODO: should we be stripping whitespace here?
        (IfMatch "*") ->
            return Nothing
        _ ->
            return $ Just ifMatch

g08 :: Monad m => FlowStateT m (Maybe IfMatch)
g08 = do
    trace "g08"
    req <- lift request
    let reqHeaders = requestHeaders req
    return $ IfMatch <$> lookup hIfMatch reqHeaders

g07 :: Monad m => Bool -> FlowStateT m Bool
g07 exists = do
    trace "g07"
    -- TODO: set Vary headers
    return exists

------------------------------------------------------------------------------
-- H column
------------------------------------------------------------------------------

h12 :: Monad m => Maybe UTCTime -> IfUnmodifiedSince -> FlowStateT m ()
h12 modified (IfUnmodifiedSince headerDate) = do
    trace "h12"
    let maybeGreater = do
            lastM <- modified
            return (lastM > headerDate)
    if maybeGreater == Just True
        then lift $ halt HTTP.status412
        else return ()

h11 :: Monad m => IfUnmodifiedSinceHeader -> FlowStateT m (Maybe IfUnmodifiedSince)
h11 (IfUnmodifiedSinceHeader header) = do
    trace "h11"
    return . fmap IfUnmodifiedSince $ parseRfc1123Date header

h10 :: Monad m => FlowStateT m (Maybe IfUnmodifiedSinceHeader)
h10 = do
    trace "h10"
    req <- lift request
    return . fmap IfUnmodifiedSinceHeader . lookup hIfUnmodifiedSince $ requestHeaders req

h07 :: Monad m => FlowStateT m ()
h07 = do
    trace "h07"
    req <- lift request
    let reqHeaders = requestHeaders req
    case lookup hIfMatch reqHeaders of
        -- TODO: should we be stripping whitespace here?
        (Just "*") ->
            lift $ halt HTTP.status412
        _ ->
          return ()

------------------------------------------------------------------------------
-- I column
------------------------------------------------------------------------------

i13 :: Monad m => IfNoneMatch -> FlowStateT m (Maybe IfNoneMatch)
i13 ifNoneMatch = do
    trace "i13"
    case ifNoneMatch of
        -- TODO: should we be stripping whitespace here?
        (IfNoneMatch "*") ->
            return Nothing
        _ ->
            return $ Just ifNoneMatch

i12 :: Monad m => FlowStateT m (Maybe IfNoneMatch)
i12 = do
    trace "i12"
    req <- lift request
    let reqHeaders = requestHeaders req
    return $ IfNoneMatch <$> lookup hIfNoneMatch reqHeaders

i07 :: Monad m => FlowStateT m Bool
i07 = do
    trace "i07"
    req <- lift request
    return $ requestMethod req == HTTP.methodPut

i04 :: Monad m => Maybe Location -> FlowStateT m ()
i04 moved = do
    trace "i04"
    case moved of
        Just (Location loc) -> do
            lift $ addResponseHeader ("Location", loc)
            lift $ halt HTTP.status301
        Nothing ->
            return ()

------------------------------------------------------------------------------
-- J column
------------------------------------------------------------------------------

j18 :: Monad m => FlowStateT m a
j18 = do
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

k13 :: Monad m => Maybe ETag -> IfNoneMatch -> FlowStateT m Bool
k13 etag (IfNoneMatch ifNoneMatch) = do
    trace "k13"
    return . any (flip F.elem etag) $ parseEtagList ifNoneMatch

k07 :: Monad m => Bool -> FlowStateT m Bool
k07 prevExisted = do
    trace "k07"
    return prevExisted

k05 :: Monad m => Maybe Location -> FlowStateT m ()
k05 moved = do
    trace "k05"
    case moved of
        Just (Location loc) -> do
            lift $ addResponseHeader ("Location", loc)
            lift $ halt HTTP.status301
        Nothing ->
            return ()

------------------------------------------------------------------------------
-- L column
------------------------------------------------------------------------------

l17 :: Monad m => Maybe UTCTime -> IfModifiedSince -> FlowStateT m ()
l17 modified (IfModifiedSince ifModifiedSince) = do
    trace "l17"
    let maybeGreater = do
            lastM <- modified
            return (lastM > ifModifiedSince)
    if maybeGreater == Just True
        then return ()
        else lift $ halt HTTP.status304

l15 :: Monad m => IfModifiedSince -> FlowStateT m Bool
l15 (IfModifiedSince ifModifiedSince) = do
    trace "l15"
    now <- lift requestTime
    return $ ifModifiedSince > now

l14 :: Monad m => IfModifiedSinceHeader -> FlowStateT m (Maybe IfModifiedSince)
l14 (IfModifiedSinceHeader dateHeader) = do
    trace "l14"
    return . fmap IfModifiedSince . parseRfc1123Date $ dateHeader

l13 :: Monad m => FlowStateT m (Maybe IfModifiedSinceHeader)
l13 = do
    trace "l13"
    req <- lift request
    let reqHeaders = requestHeaders req
    return . fmap IfModifiedSinceHeader $ lookup HTTP.hIfModifiedSince reqHeaders

l07 :: Monad m => FlowStateT m ()
l07 = do
    trace "l07"
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then return ()
        else lift $ halt HTTP.status404

l05 :: Monad m => Maybe Location -> FlowStateT m ()
l05 moved = do
    trace "l05"
    case moved of
        Just (Location loc) -> do
            lift $ addResponseHeader ("Location", loc)
            lift $ halt HTTP.status307
        Nothing ->
            return ()

------------------------------------------------------------------------------
-- M column
------------------------------------------------------------------------------

m20 :: Monad m => Bool -> FlowStateT m ()
m20 completed = do
    trace "m20"
    if completed
        then return ()
        else lift $ halt HTTP.status202

m16 :: Monad m => FlowStateT m Bool
m16 = do
    trace "m16"
    req <- lift request
    return $ requestMethod req == HTTP.methodDelete

m07 :: Monad m => Bool -> FlowStateT m ()
m07 allowMissing = do
    trace "m07"
    if allowMissing
        then return ()
        else lift $ halt HTTP.status404

m05 :: Monad m => FlowStateT m ()
m05 = do
    trace "m05"
    req <- lift request
    if requestMethod req == HTTP.methodPost
        then return ()
        else lift $ halt HTTP.status410

------------------------------------------------------------------------------
-- N column
------------------------------------------------------------------------------

n16 :: Monad m => FlowStateT m Bool
n16 = do
    trace "n16"
    req <- lift request
    return $ requestMethod req == HTTP.methodPost

n11 :: Monad m => Maybe Location -> FlowStateT m ()
n11 location = do
    trace "n11"
    case location of
        Just (Location loc) -> do
            lift $ setResponseHeader (HTTP.hLocation, loc)
            lift $ halt HTTP.status303
        _ ->
            return ()

n05 :: Monad m => Bool -> FlowStateT m ()
n05 allow = do
    trace "n05"
    if allow
        then return ()
        else lift $ halt HTTP.status410

------------------------------------------------------------------------------
-- O column
------------------------------------------------------------------------------

o20 :: Monad m => ResponseBody -> FlowStateT m ()
o20 body = do
    trace "o20"
    -- ResponseBody is a little tough to make an instance of 'Eq',
    -- so we just use a pattern match
    case body of
        Empty   -> lift $ halt HTTP.status204
        _       -> return ()

o18 :: Monad m => Bool -> CacheData -> MediaType -> Webmachine m ResponseBody -> FlowStateT m a
o18 multiple (CacheData modified' etag') cType body' = do
    trace "o18"
    if multiple
        -- TODO I believe this can/should also set the body
        then lift $ halt HTTP.status300
        else do
            body <- lift body'
            -- TODO: set etag, expiration, etc. headers
            req <- lift request
            let getOrHead = [ HTTP.methodGet
                            , HTTP.methodHead
                            ]
            when (requestMethod req `elem` getOrHead) $ do
                lift $ putResponseBody body
                lift $ addResponseHeader ("Content-Type", renderHeader cType)
            lift . F.forM_ modified' $ addResponseHeader . (,) "Last-Modified" . utcTimeToRfc1123
            lift . F.forM_ etag' $ addResponseHeader . (,) "ETag" . etagToByteString
            lift $ halt HTTP.status200

o16 :: Monad m => FlowStateT m Bool
o16 = do
    trace "o16"
    req <- lift request
    return $ requestMethod req == HTTP.methodPut

o14 :: Monad m => Bool -> FlowStateT m ()
o14 conflict = do
    trace "o14"
    if conflict
        then lift $ halt HTTP.status409
        else return ()

------------------------------------------------------------------------------
-- P column
------------------------------------------------------------------------------


p11 :: Monad m => Maybe Location -> FlowStateT m ()
p11 location = do
    trace "p11"
    case location of
        Just (Location loc) -> do
            lift $ setResponseHeader (HTTP.hLocation, loc)
            lift $ halt HTTP.status201
        _ ->
            return ()

p03 :: Monad m => Bool -> FlowStateT m ()
p03 conflict = do
    trace "p03"
    if conflict
        then lift $ halt HTTP.status409
        else return ()
