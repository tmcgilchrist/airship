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
    , hoistResource
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
data PostResponse m
    = PostCreate [Text] -- ^ Treat this request as a PUT.
    | PostCreateRedirect [Text] -- ^ Treat this request as a PUT, then redirect.
    | PostProcess (Handler m ()) -- ^ Process as a POST, but don't redirect.
    | PostProcessRedirect (Handler m ByteString) -- ^ Process and redirect.

-- PostResponse can't be an MFunctor because it's missing a type hole
hoistPostResponse :: Monad m => (forall a. m a -> n a) -> PostResponse m -> PostResponse n
hoistPostResponse _ (PostCreate t) = PostCreate t
hoistPostResponse _ (PostCreateRedirect t) = PostCreateRedirect t
hoistPostResponse nat (PostProcess h) = PostProcess (hoist nat h)
hoistPostResponse nat (PostProcessRedirect h) = PostProcessRedirect (hoist nat h)

data Resource m =
    Resource { -- | Whether to allow HTTP POSTs to a missing resource. Default: false.
               allowMissingPost         :: Handler m Bool
               -- | The set of HTTP methods that this resource allows. Default: @GET@ and @HEAD@.
               -- If a request arrives with an HTTP method not included herein, @501 Not Implemented@ is returned.
             , allowedMethods           :: Handler m [Method]
               -- | An association list of 'MediaType's and 'Handler' actions that correspond to the accepted
               -- @Content-Type@ values that this resource can accept in a request body. If a @Content-Type@ header
               -- is present but not accounted for in 'contentTypesAccepted', processing will halt with @415 Unsupported Media Type@.
               -- Otherwise, the corresponding 'Webmachine' action will be executed and processing will continue.
             , contentTypesAccepted     :: Handler m [(MediaType, Webmachine m ())]
               -- | An association list of 'MediaType' values and 'ResponseBody' values. The response will be chosen
               -- by looking up the 'MediaType' that most closely matches the @Content-Type@ header. Should there be no match,
               -- processing will halt with @406 Not Acceptable@.
             , contentTypesProvided     :: Handler m [(MediaType, Webmachine m ResponseBody)]
               -- | When a @DELETE@ request is enacted (via a @True@ value returned from 'deleteResource'), a
               -- @False@ value returns a @202 Accepted@ response. Returning @True@ will continue processing,
               -- usually ending up with a @204 No Content@ response. Default: False.
             , deleteCompleted          :: Handler m Bool
               -- | When processing a @DELETE@ request, a @True@ value allows processing to continue.
               -- Returns @500 Forbidden@ if False. Default: false.
             , deleteResource           :: Handler m Bool
               -- | Returns @413 Request Entity Too Large@ if true. Default: false.
             , entityTooLarge           :: Handler m Bool
               -- | Checks if the given request is allowed to access this resource.
               -- Returns @403 Forbidden@ if true. Default: false.
             , forbidden                :: Handler m Bool
               -- | If this returns a non-'Nothing' 'ETag', its value will be added to every HTTP response
               -- in the @ETag:@ field.
             , generateETag             :: Handler m (Maybe ETag)
               -- | Checks if this resource has actually implemented a handler for a given HTTP method.
               -- Returns @501 Not Implemented@ if false. Default: true.
             , implemented              :: Handler m Bool
               -- | Returns @401 Unauthorized@ if false. Default: true.
             , isAuthorized             :: Handler m Bool
               -- | When processing @PUT@ requests, a @True@ value returned here will halt processing with a @409 Created@.
             , isConflict               :: Handler m Bool
               -- | Returns @415 Unsupported Media Type@ if false. We recommend you use the 'contentTypeMatches' helper function, which accepts a list of
               -- 'MediaType' values, so as to simplify proper MIME type handling. Default: true.
             , knownContentType         :: Handler m Bool
               -- | In the presence of an @If-Modified-Since@ header, returning a @Just@ value from 'lastModifed' allows
               -- the server to halt with @304 Not Modified@ if appropriate.
             , lastModified             :: Handler m (Maybe UTCTime)
               -- | If an @Accept-Language@ value is present in the HTTP request, and this function returns @False@,
               -- processing will halt with @406 Not Acceptable@.
             , languageAvailable        :: Handler m Bool
               -- | Returns @400 Bad Request@ if true. Default: false.
             , malformedRequest         :: Handler m Bool
                                        -- wondering if this should be text,
                                        -- or some 'path' type
               -- | When processing a resource for which 'resourceExists' returned @False@, returning a @Just@ value
               -- halts with a @301 Moved Permanently@ response. The contained 'ByteString' will be added to the
               -- HTTP response under the @Location:@ header.
             , movedPermanently         :: Handler m (Maybe ByteString)
               -- | Like 'movedPermanently', except with a @307 Moved Temporarily@ response.
             , movedTemporarily         :: Handler m (Maybe ByteString)
               -- | When handling a @PUT@ request, returning @True@ here halts processing with @300 Multiple Choices@. Default: False.
             , multipleChoices          :: Handler m Bool
               -- | When processing a request for which 'resourceExists' returned @False@, returning @True@ here
               -- allows the 'movedPermanently' and 'movedTemporarily' functions to process the request.
             , previouslyExisted        :: Handler m Bool
               -- | When handling @POST@ requests, the value returned determines whether to treat the request as a @PUT@,
               -- a @PUT@ and a redirect, or a plain @POST@. See the documentation for 'PostResponse' for more information.
               -- The default implemetation returns a 'PostProcess' with an empty handler.
             , processPost              :: Handler m (PostResponse m)
               -- | Does the resource at this path exist?
               -- Returning false from this usually entails a @404 Not Found@ response.
               -- (If 'allowMissingPost' returns @True@ or an @If-Match: *@ header is present, it may not).
             , resourceExists           :: Handler m Bool
               -- | Returns @503 Service Unavailable@ if false. Default: true.
             , serviceAvailable         :: Handler m Bool
               -- | Returns @414 Request URI Too Long@ if true. Default: false.
             , uriTooLong               :: Handler m Bool
               -- | Returns @501 Not Implemented@ if false. Default: true.
             , validContentHeaders      :: Handler m Bool
             }

-- | A helper function that terminates execution with @500 Internal Server Error@.
serverError :: Handler m a
serverError = finishWith (Response status500 [] Empty)

-- | The default Airship resource, with "sensible" values filled in for each entry.
-- You construct new resources by extending the default resource with your own handlers.
defaultResource :: Resource m
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

-- Resource can't be an MFunctor because it's missing a type hole
hoistResource :: Monad m => (forall a. m a -> n a) -> Resource m -> Resource n
hoistResource nat r =
   Resource {
     allowMissingPost       = hoist nat (allowMissingPost r)
   , allowedMethods         = hoist nat (allowedMethods r)
   , contentTypesAccepted   = fmap (fmap (\(mt, h) -> (mt, hoist nat h))) $ hoist nat (contentTypesAccepted r)
   , contentTypesProvided   = fmap (fmap (\(mt, h) -> (mt, hoist nat h))) $ hoist nat (contentTypesProvided r)
   , deleteCompleted        = hoist nat (deleteCompleted r)
   , deleteResource         = hoist nat (deleteResource r)
   , entityTooLarge         = hoist nat (entityTooLarge r)
   , forbidden              = hoist nat (forbidden r)
   , generateETag           = hoist nat (generateETag r)
   , implemented            = hoist nat (implemented r)
   , isAuthorized           = hoist nat (isAuthorized r)
   , isConflict             = hoist nat (isConflict r)
   , knownContentType       = hoist nat (knownContentType r)
   , lastModified           = hoist nat (lastModified r)
   , languageAvailable      = hoist nat (languageAvailable r)
   , malformedRequest       = hoist nat (malformedRequest r)
   , movedPermanently       = hoist nat (movedPermanently r)
   , movedTemporarily       = hoist nat (movedTemporarily r)
   , multipleChoices        = hoist nat (multipleChoices r)
   , previouslyExisted      = hoist nat (previouslyExisted r)
   , processPost            = fmap (hoistPostResponse nat) $ hoist nat (processPost r)
   , resourceExists         = hoist nat (resourceExists r)
   , serviceAvailable       = hoist nat (serviceAvailable r)
   , uriTooLong             = hoist nat (uriTooLong r)
   , validContentHeaders    = hoist nat (validContentHeaders r)
   }
