{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Airship.Resource
    ( Resource(..)
    , PostResponse(..)
    , defaultResource
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
    | PostProcess (Webmachine m ()) -- ^ Process as a POST, but don't redirect.
    | PostProcessRedirect (Webmachine m ByteString) -- ^ Process and redirect.

data Resource m =
    Resource { -- | Whether to allow HTTP POSTs to a missing resource. Default: false.
               allowMissingPost         :: Webmachine m Bool
               -- | The set of HTTP methods that this resource allows. Default: @GET@ and @HEAD@.
               -- If a request arrives with an HTTP method not included herein, @501 Not Implemented@ is returned.
             , allowedMethods           :: Webmachine m [Method]
               -- | An association list of 'MediaType's and 'Webmachine' actions that correspond to the accepted
               -- @Content-Type@ values that this resource can accept in a request body. If a @Content-Type@ header
               -- is present but not accounted for in 'contentTypesAccepted', processing will halt with @415 Unsupported Media Type@.
               -- Otherwise, the corresponding 'Webmachine' action will be executed and processing will continue.
             , contentTypesAccepted     :: Webmachine m [(MediaType, Webmachine m ())]
               -- | An association list of 'MediaType' values and 'ResponseBody' values. The response will be chosen
               -- by looking up the 'MediaType' that most closely matches the @Accept@ header. Should there be no match,
               -- processing will halt with @406 Not Acceptable@.
             , contentTypesProvided     :: Webmachine m [(MediaType, Webmachine m ResponseBody)]
               -- | When a @DELETE@ request is enacted (via a @True@ value returned from 'deleteResource'), a
               -- @False@ value returns a @202 Accepted@ response. Returning @True@ will continue processing,
               -- usually ending up with a @204 No Content@ response. Default: False.
             , deleteCompleted          :: Webmachine m Bool
               -- | When processing a @DELETE@ request, a @True@ value allows processing to continue.
               -- Returns @500 Forbidden@ if False. Default: false.
             , deleteResource           :: Webmachine m Bool
               -- | Returns @413 Request Entity Too Large@ if true. Default: false.
             , entityTooLarge           :: Webmachine m Bool
               -- | Checks if the given request is allowed to access this resource.
               -- Returns @403 Forbidden@ if true. Default: false.
             , forbidden                :: Webmachine m Bool
               -- | If this returns a non-'Nothing' 'ETag', its value will be added to every HTTP response
               -- in the @ETag:@ field.
             , generateETag             :: Webmachine m (Maybe ETag)
               -- | Checks if this resource has actually implemented a handler for a given HTTP method.
               -- Returns @501 Not Implemented@ if false. Default: true.
             , implemented              :: Webmachine m Bool
               -- | Returns @401 Unauthorized@ if false. Default: true.
             , isAuthorized             :: Webmachine m Bool
               -- | When processing @PUT@ requests, a @True@ value returned here will halt processing with a @409 Conflict@.
             , isConflict               :: Webmachine m Bool
               -- | In the presence of an @If-Modified-Since@ header, returning a @Just@ value from 'lastModifed' allows
               -- the server to halt with @304 Not Modified@ if appropriate.
             , lastModified             :: Webmachine m (Maybe UTCTime)
               -- | If an @Accept-Language@ value is present in the HTTP request, and this function returns @False@,
               -- processing will halt with @406 Not Acceptable@.
             , languageAvailable        :: Webmachine m Bool
               -- | Returns @400 Bad Request@ if true. Default: false.
             , malformedRequest         :: Webmachine m Bool
                                        -- wondering if this should be text,
                                        -- or some 'path' type
               -- | When processing a resource for which 'resourceExists' returned @False@, returning a @Just@ value
               -- halts with a @301 Moved Permanently@ response. The contained 'ByteString' will be added to the
               -- HTTP response under the @Location:@ header.
             , movedPermanently         :: Webmachine m (Maybe ByteString)
               -- | Like 'movedPermanently', except with a @307 Moved Temporarily@ response.
             , movedTemporarily         :: Webmachine m (Maybe ByteString)
               -- | When handling a @PUT@ request, returning @True@ here halts processing with @300 Multiple Choices@. Default: False.
             , multipleChoices          :: Webmachine m Bool
               -- | When processing a request for which 'resourceExists' returned @False@, returning @True@ here
               -- allows the 'movedPermanently' and 'movedTemporarily' functions to process the request.
             , previouslyExisted        :: Webmachine m Bool
               -- | When handling @POST@ requests, the value returned determines whether to treat the request as a @PUT@,
               -- a @PUT@ and a redirect, or a plain @POST@. See the documentation for 'PostResponse' for more information.
               -- The default implemetation returns a 'PostProcess' with an empty handler.
             , processPost              :: Webmachine m (PostResponse m)
               -- | Does the resource at this path exist?
               -- Returning false from this usually entails a @404 Not Found@ response.
               -- (If 'allowMissingPost' returns @True@ or an @If-Match: *@ header is present, it may not).
             , resourceExists           :: Webmachine m Bool
               -- | Returns @503 Service Unavailable@ if false. Default: true.
             , serviceAvailable         :: Webmachine m Bool
               -- | Returns @414 Request URI Too Long@ if true. Default: false.
             , uriTooLong               :: Webmachine m Bool
               -- | Returns @501 Not Implemented@ if false. Default: true.
             , validContentHeaders      :: Webmachine m Bool
             }

-- | The default Airship resource, with "sensible" values filled in for each entry.
-- You construct new resources by extending the default resource with your own handlers.
defaultResource :: Monad m => Resource m
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
