{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Airship.Resource2.Data
  ( Available (..)
  , MOtherAction (..)
  , MPostAction (..)
  , MPutAction (..)
  , Moved (..)
  , MultipleRepresentations (..)
  , OctetStream (..)
  , Resource
  , ResourceDenied (..)
  , ResourceDesc (..)
  , ResponseEntity (..)
  , ResourceExists (..)
  , ResourceMethod (..)
  , ResourceNotFoundMethod (..)
  , RDeleteAction (..)
  , RPostAction (..)
  , RPutAction (..)
  , ok
  , resourceDesc
  ) where

import           Airship.Internal.Decision
import           Airship.Types

import           Data.List.NonEmpty (NonEmpty (..))

import           Network.HTTP.Media (MediaType)
import           Network.HTTP.Types (Method)
import qualified Network.HTTP.Types as HTTP


data ResourceDesc a b =
  ResourceDesc {
    serviceAvailable :: Available
  , allowedMethods :: [Method]
  , contentTypesAccepted :: NonEmpty (MediaType, a)
  , contentTypesProvided :: NonEmpty (MediaType, b)
  , languageAvailable :: AcceptLanguage -> Bool
  }

data ResourceDenied =
    Forbidden
  | Unauthorized
  | MalformedRequest
  | UnsupportedContentType
  | UriTooLong
  | EntityTooLarge
  deriving (Eq, Show)

data Available =
    Available
  | Unavailable
  deriving (Eq, Show)

data MultipleRepresentations =
    MultipleRepresentations
  | OK
  deriving (Eq, Show)

data Moved =
    MovedPermanently Location
  | MovedTemporarily Location
  deriving (Eq, Show)

data ResponseEntity =
    ResponseEntity MultipleRepresentations ResponseBody


type Resource m = FlowStateT m ()


data ResourceExists m =
    ResourceExists CacheData (ResourceMethod m -> m ())
  | ResourceNotFound (ResourceNotFoundMethod m -> m ())

data ResourceMethod m =
    RPut (RPutAction -> m ())
  | RPost (RPostAction -> m ())
  | RDelete (RDeleteAction -> m ())
  | ROther (ResponseEntity -> m ())

data ResourceNotFoundMethod m =
    MPut (MPutAction -> m ())
  | MPost (MPostAction -> m ())
  | MOther (MOtherAction -> m ())

data RPutAction =
    RPutConflict
  | RPutCreated Location
  | RPutOk ResponseEntity

data RPostAction =
    RPostSeeOther Location
  | RPostCreated Location
  | RPostOk ResponseEntity

data RDeleteAction =
    RDeleteAccepted
  | RDeleteOk ResponseEntity

data MPutAction =
    MPutMoved Location
  | MPutConflict
  | MPutCreated Location
  | MPutOk CacheData ResponseEntity

data MPostAction =
    MPostMoved Moved
  | MPostGone
  | MPostNotFound
  | MPostSeeOther Location
  | MPostRedirect Location
  | MPostOk CacheData ResponseEntity

data MOtherAction =
    MOtherMoved Moved
  | MOtherGone
  | MOtherNotFound


data OctetStream = OctetStream deriving (Eq, Show)


resourceDesc :: ResourceDesc OctetStream OctetStream
resourceDesc =
    ResourceDesc
        Available
        [HTTP.methodGet, HTTP.methodHead]
        (("application/octet-stream", OctetStream) :| [])
        (("application/octet-stream", OctetStream) :| [])
        (const True)

ok :: ResponseBody -> ResponseEntity
ok =
    ResponseEntity OK
