{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( Resource(..)
    , PostResponse(..)
    , serverError
    , defaultResource
    , setResourceParams
    ) where

import Airship.Types

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)

import Network.HTTP.Types
import Network.HTTP.Media (MediaType)

-- | Used when processing POST requests so as to handle the outcome of the binary decisions between
-- handling a POST as a create request and whether to redirect after the POST is done.
-- Credit for this idea goes to Richard Wallace (purefn) on Webcrank.
data PostResponse p m
    = PostCreate [Text] -- ^ Treat this request as a PUT.
    | PostCreateRedirect [Text] -- ^ Treat this request as a PUT, then redirect.
    | PostProcess (Webmachine p m ()) -- ^ Process as a POST, but don't redirect.
    | PostProcessRedirect (Webmachine p m ByteString) -- ^ Process and redirect.

data Resource p m =
    Resource { -- | Whether to allow HTTP POSTs to a missing resource. Default: false.
               allowMissingPost         :: Webmachine p m Bool
               -- | The set of HTTP methods that this resource allows. Default: @GET@ and @HEAD@.
               -- If a request arrives with an HTTP method not included herein, @501 Not Implemented@ is returned.
             , allowedMethods           :: Webmachine p m [Method]
               -- | An association list of 'MediaType's and 'Webmachine' actions that correspond to the accepted
               -- @Content-Type@ values that this resource can accept in a request body. If a @Content-Type@ header
               -- is present but not accounted for in 'contentTypesAccepted', processing will halt with @415 Unsupported Media Type@.
               -- Otherwise, the corresponding 'Webmachine' action will be executed and processing will continue.
             , contentTypesAccepted     :: Webmachine p m [(MediaType, Webmachine p m ())]
               -- | An association list of 'MediaType' values and 'ResponseBody' values. The response will be chosen
               -- by looking up the 'MediaType' that most closely matches the @Accept@ header. Should there be no match,
               -- processing will halt with @406 Not Acceptable@.
             , contentTypesProvided     :: Webmachine p m [(MediaType, Webmachine p m ResponseBody)]
               -- | When a @DELETE@ request is enacted (via a @True@ value returned from 'deleteResource'), a
               -- @False@ value returns a @202 Accepted@ response. Returning @True@ will continue processing,
               -- usually ending up with a @204 No Content@ response. Default: False.
             , deleteCompleted          :: Webmachine p m Bool
               -- | When processing a @DELETE@ request, a @True@ value allows processing to continue.
               -- Returns @500 Forbidden@ if False. Default: false.
             , deleteResource           :: Webmachine p m Bool
               -- | Returns @413 Request Entity Too Large@ if true. Default: false.
             , entityTooLarge           :: Webmachine p m Bool
               -- | Checks if the given request is allowed to access this resource.
               -- Returns @403 Forbidden@ if true. Default: false.
             , forbidden                :: Webmachine p m Bool
               -- | If this returns a non-'Nothing' 'ETag', its value will be added to every HTTP response
               -- in the @ETag:@ field.
             , generateETag             :: Webmachine p m (Maybe ETag)
               -- | Checks if this resource has actually implemented a handler for a given HTTP method.
               -- Returns @501 Not Implemented@ if false. Default: true.
             , implemented              :: Webmachine p m Bool
               -- | Returns @401 Unauthorized@ if false. Default: true.
             , isAuthorized             :: Webmachine p m Bool
               -- | When processing @PUT@ requests, a @True@ value returned here will halt processing with a @409 Conflict@.
             , isConflict               :: Webmachine p m Bool
               -- | Returns @415 Unsupported Media Type@ if false. We recommend you use the 'contentTypeMatches' helper function, which accepts a list of
               -- 'MediaType' values, so as to simplify proper MIME type handling. Default: true.
             , knownContentType         :: Webmachine p m Bool
               -- | In the presence of an @If-Modified-Since@ header, returning a @Just@ value from 'lastModifed' allows
               -- the server to halt with @304 Not Modified@ if appropriate.
             , lastModified             :: Webmachine p m (Maybe UTCTime)
               -- | If an @Accept-Language@ value is present in the HTTP request, and this function returns @False@,
               -- processing will halt with @406 Not Acceptable@.
             , languageAvailable        :: Webmachine p m Bool
               -- | Returns @400 Bad Request@ if true. Default: false.
             , malformedRequest         :: Webmachine p m Bool
                                        -- wondering if this should be text,
                                        -- or some 'path' type
               -- | When processing a resource for which 'resourceExists' returned @False@, returning a @Just@ value
               -- halts with a @301 Moved Permanently@ response. The contained 'ByteString' will be added to the
               -- HTTP response under the @Location:@ header.
             , movedPermanently         :: Webmachine p m (Maybe ByteString)
               -- | Like 'movedPermanently', except with a @307 Moved Temporarily@ response.
             , movedTemporarily         :: Webmachine p m (Maybe ByteString)
               -- | When handling a @PUT@ request, returning @True@ here halts processing with @300 Multiple Choices@. Default: False.
             , multipleChoices          :: Webmachine p m Bool
               -- | When processing a request for which 'resourceExists' returned @False@, returning @True@ here
               -- allows the 'movedPermanently' and 'movedTemporarily' functions to process the request.
             , previouslyExisted        :: Webmachine p m Bool
               -- | When handling @POST@ requests, the value returned determines whether to treat the request as a @PUT@,
               -- a @PUT@ and a redirect, or a plain @POST@. See the documentation for 'PostResponse' for more information.
               -- The default implemetation returns a 'PostProcess' with an empty handler.
             , processPost              :: Webmachine p m (PostResponse p m)
               -- | Does the resource at this path exist?
               -- Returning false from this usually entails a @404 Not Found@ response.
               -- (If 'allowMissingPost' returns @True@ or an @If-Match: *@ header is present, it may not).
             , resourceExists           :: Webmachine p m Bool
               -- | Returns @503 Service Unavailable@ if false. Default: true.
             , serviceAvailable         :: Webmachine p m Bool
               -- | Returns @414 Request URI Too Long@ if true. Default: false.
             , uriTooLong               :: Webmachine p m Bool
               -- | Returns @501 Not Implemented@ if false. Default: true.
             , validContentHeaders      :: Webmachine p m Bool
             }

-- | A helper function that terminates execution with @500 Internal Server Error@.
serverError :: Monad m => Webmachine p m a
serverError = finishWith (Response status500 [] Empty)

-- | The default Airship resource, with "sensible" values filled in for each entry.
-- You construct new resources by extending the default resource with your own handlers.
defaultResource :: Monad m => Resource p m
defaultResource = Resource { allowMissingPost       = return False
                           , allowedMethods         = return [methodGet, methodHead]
                           , contentTypesAccepted   = return []
                           , contentTypesProvided   = return []
                           , deleteCompleted        = return False
                           , deleteResource         = return False
                           , entityTooLarge         = return False
                           , forbidden              = return False
                           , generateETag           = return Nothing
                           , implemented            = return True
                           , isAuthorized           = return True
                           , isConflict             = return False
                           , knownContentType       = return True
                           , lastModified           = return Nothing
                           , languageAvailable      = return True
                           , malformedRequest       = return False
                           , movedPermanently       = return Nothing
                           , movedTemporarily       = return Nothing
                           , multipleChoices        = return False
                           , previouslyExisted      = return False
                           , processPost            = return (PostProcess (return ()))
                           , resourceExists         = return True
                           , serviceAvailable       = return True
                           , uriTooLong             = return False
                           , validContentHeaders    = return True
                           }

setResourceParams :: Monad m => p -> Resource p m -> Resource p' m
setResourceParams p r =
   let ap = setParams p
   in Resource {
         allowMissingPost       = ap (allowMissingPost r)
       , allowedMethods         = ap (allowedMethods r)
       , contentTypesAccepted   = ap $ fmap (fmap (\(mt, h) -> (mt, ap h))) (contentTypesAccepted r)
       , contentTypesProvided   = ap $ fmap (fmap (\(mt, h) -> (mt, ap h))) (contentTypesProvided r)
       , deleteCompleted        = ap (deleteCompleted r)
       , deleteResource         = ap (deleteResource r)
       , entityTooLarge         = ap (entityTooLarge r)
       , forbidden              = ap (forbidden r)
       , generateETag           = ap (generateETag r)
       , implemented            = ap (implemented r)
       , isAuthorized           = ap (isAuthorized r)
       , isConflict             = ap (isConflict r)
       , knownContentType       = ap (knownContentType r)
       , lastModified           = ap (lastModified r)
       , languageAvailable      = ap (languageAvailable r)
       , malformedRequest       = ap (malformedRequest r)
       , movedPermanently       = ap (movedPermanently r)
       , movedTemporarily       = ap (movedTemporarily r)
       , multipleChoices        = ap (multipleChoices r)
       , previouslyExisted      = ap (previouslyExisted r)
       , processPost            = ap $ fmap (setPostResponseParams p) (processPost r)
       , resourceExists         = ap (resourceExists r)
       , serviceAvailable       = ap (serviceAvailable r)
       , uriTooLong             = ap (uriTooLong r)
       , validContentHeaders    = ap (validContentHeaders r)
       }

setPostResponseParams :: Monad m => p -> PostResponse p m -> PostResponse p' m
setPostResponseParams _ (PostCreate t) = PostCreate t
setPostResponseParams _ (PostCreateRedirect t) = PostCreateRedirect t
setPostResponseParams r (PostProcess h) = PostProcess (setParams r h)
setPostResponseParams r (PostProcessRedirect h) = PostProcessRedirect (setParams r h)
