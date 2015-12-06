{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Airship.Resource
    ( Resource(..)
    , PostResponse(..)
    , serverError
    , defaultResource
    , allowMissingPost
    , allowedMethods
    , contentTypesAccepted
    , contentTypesProvided
    , deleteCompleted
    , deleteResource
    , entityTooLarge
    , forbidden
    , generateETag
    , implemented
    , isAuthorized
    , isConflict
    , knownContentType
    , languageAvailable
    , lastModified
    , malformedRequest
    , movedPermanently
    , movedTemporarily
    , multipleChoices
    , previouslyExisted
    , processPost
    , resourceExists
    , serviceAvailable
    , uriTooLong
    , validContentHeaders
    ) where

import Airship.Types

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)

import Lens.Micro.TH (makeLenses)

import Network.HTTP.Types
import Network.HTTP.Media (MediaType)

-- | Used when processing POST requests so as to handle the outcome of the binary decisions between
-- handling a POST as a create request and whether to redirect after the POST is done.
-- Credit for this idea goes to Richard Wallace (purefn) on Webcrank.
data PostResponse m
    = PostCreate [Text] -- ^ Treat this request as a PUT.
    | PostCreateRedirect [Text] -- ^ Treat this request as a PUT, then redirect.
    | PostProcess (Webmachine m ()) -- ^ Process as a POST, but don't redirect.
    | PostProcessRedirect (Webmachine m ByteString) -- ^ Process and redirect.

data Resource m =
    Resource { -- | Whether to allow HTTP POSTs to a missing resource. Default: false.
               _allowMissingPost         :: Webmachine m Bool
               -- | The set of HTTP methods that this resource allows. Default: @GET@ and @HEAD@.
               -- If a request arrives with an HTTP method not included herein, @501 Not Implemented@ is returned.
             , _allowedMethods           :: Webmachine m [Method]
               -- | An association list of 'MediaType's and 'Webmachine' actions that correspond to the accepted
               -- @Content-Type@ values that this resource can accept in a request body. If a @Content-Type@ header
               -- is present but not accounted for in 'contentTypesAccepted', _processing will halt with @415 Unsupported Media Type@.
               -- Otherwise, _the corresponding 'Webmachine' action will be executed and processing will continue.
             , _contentTypesAccepted     :: Webmachine m [(MediaType, Webmachine m ())]
               -- | An association list of 'MediaType' values and 'ResponseBody' values. The response will be chosen
               -- by looking up the 'MediaType' that most closely matches the @Content-Type@ header. Should there be no match,
               -- processing will halt with @406 Not Acceptable@.
             , _contentTypesProvided     :: Webmachine m [(MediaType, Webmachine m ResponseBody)]
               -- | When a @DELETE@ request is enacted (via a @True@ value returned from 'deleteResource'), _a
               -- @False@ value returns a @202 Accepted@ response. Returning @True@ will continue processing,
               -- usually ending up with a @204 No Content@ response. Default: False.
             , _deleteCompleted          :: Webmachine m Bool
               -- | When processing a @DELETE@ request, _a @True@ value allows processing to continue.
               -- Returns @500 Forbidden@ if False. Default: false.
             , _deleteResource           :: Webmachine m Bool
               -- | Returns @413 Request Entity Too Large@ if true. Default: false.
             , _entityTooLarge           :: Webmachine m Bool
               -- | Checks if the given request is allowed to access this resource.
               -- Returns @403 Forbidden@ if true. Default: false.
             , _forbidden                :: Webmachine m Bool
               -- | If this returns a non-'Nothing' 'ETag', _its value will be added to every HTTP response
               -- in the @ETag:@ field.
             , _generateETag             :: Webmachine m (Maybe ETag)
               -- | Checks if this resource has actually implemented a handler for a given HTTP method.
               -- Returns @501 Not Implemented@ if false. Default: true.
             , _implemented              :: Webmachine m Bool
               -- | Returns @401 Unauthorized@ if false. Default: true.
             , _isAuthorized             :: Webmachine m Bool
               -- | When processing @PUT@ requests, _a @True@ value returned here will halt processing with a @409 Created@.
             , _isConflict               :: Webmachine m Bool
               -- | Returns @415 Unsupported Media Type@ if false. We recommend you use the 'contentTypeMatches' helper function, _which accepts a list of
               -- 'MediaType' values, _so as to simplify proper MIME type handling. Default: true.
             , _knownContentType         :: Webmachine m Bool
               -- | In the presence of an @If-Modified-Since@ header, _returning a @Just@ value from 'lastModifed' allows
               -- the server to halt with @304 Not Modified@ if appropriate.
             , _lastModified             :: Webmachine m (Maybe UTCTime)
               -- | If an @Accept-Language@ value is present in the HTTP request, _and this function returns @False@,
               -- processing will halt with @406 Not Acceptable@.
             , _languageAvailable        :: Webmachine m Bool
               -- | Returns @400 Bad Request@ if true. Default: false.
             , _malformedRequest         :: Webmachine m Bool
                                        -- wondering if this should be text,
                                        -- or some 'path' type
               -- | When processing a resource for which 'resourceExists' returned @False@, _returning a @Just@ value
               -- halts with a @301 Moved Permanently@ response. The contained 'ByteString' will be added to the
               -- HTTP response under the @Location:@ header.
             , _movedPermanently         :: Webmachine m (Maybe ByteString)
               -- | Like 'movedPermanently', _except with a @307 Moved Temporarily@ response.
             , _movedTemporarily         :: Webmachine m (Maybe ByteString)
               -- | When handling a @PUT@ request, _returning @True@ here halts processing with @300 Multiple Choices@. Default: False.
             , _multipleChoices          :: Webmachine m Bool
               -- | When processing a request for which 'resourceExists' returned @False@, returning @True@ here
               -- allows the 'movedPermanently' and 'movedTemporarily' functions to process the request.
             , _previouslyExisted        :: Webmachine m Bool
               -- | When handling @POST@ requests, the value returned determines whether to treat the request as a @PUT@,
               -- a @PUT@ and a redirect, or a plain @POST@. See the documentation for 'PostResponse' for more information.
               -- The default implemetation returns a 'PostProcess' with an empty handler.
             , _processPost              :: Webmachine m (PostResponse m)
               -- | Does the resource at this path exist?
               -- Returning false from this usually entails a @404 Not Found@ response.
               -- (If 'allowMissingPost' returns @True@ or an @If-Match: *@ header is present, it may not).
             , _resourceExists           :: Webmachine m Bool
               -- | Returns @503 Service Unavailable@ if false. Default: true.
             , _serviceAvailable         :: Webmachine m Bool
               -- | Returns @414 Request URI Too Long@ if true. Default: false.
             , _uriTooLong               :: Webmachine m Bool
               -- | Returns @501 Not Implemented@ if false. Default: true.
             , _validContentHeaders      :: Webmachine m Bool
             }

makeLenses ''Resource

-- | A helper function that terminates execution with @500 Internal Server Error@.
serverError :: Monad m => Webmachine m a
serverError = finishWith (Response status500 [] Empty)

-- | The default Airship resource, with "sensible" values filled in for each entry.
-- You construct new resources by extending the default resource with your own handlers.
defaultResource :: Monad m => Resource m
defaultResource = Resource { _allowMissingPost       = return False
                           , _allowedMethods         = return [methodGet, methodHead]
                           , _contentTypesAccepted   = return []
                           , _contentTypesProvided   = return []
                           , _deleteCompleted        = return False
                           , _deleteResource         = return False
                           , _entityTooLarge         = return False
                           , _forbidden              = return False
                           , _generateETag           = return Nothing
                           , _implemented            = return True
                           , _isAuthorized           = return True
                           , _isConflict             = return False
                           , _knownContentType       = return True
                           , _lastModified           = return Nothing
                           , _languageAvailable      = return True
                           , _malformedRequest       = return False
                           , _movedPermanently       = return Nothing
                           , _movedTemporarily       = return Nothing
                           , _multipleChoices        = return False
                           , _previouslyExisted      = return False
                           , _processPost            = return (PostProcess (return ()))
                           , _resourceExists         = return True
                           , _serviceAvailable       = return True
                           , _uriTooLong             = return False
                           , _validContentHeaders    = return True
                           }
