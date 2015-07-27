{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Airship
  ( module Airship.Resource
  , module Airship.Headers
  , module Airship.Helpers
  , module Airship.Route
  , module Airship.Types
  , redirectTemporarily
  , redirectPermanently
  ) where

import           Airship.Headers
import           Airship.Helpers
import           Airship.Resource
import           Airship.Route
import           Airship.Types

import qualified Network.HTTP.Types as HTTP
import           Data.ByteString (ByteString)

-- | Issue an HTTP 302 (Found) response, with `location' as the destination.
redirectTemporarily :: ByteString -> Handler s m a
redirectTemporarily location =
    addResponseHeader ("Location", location) >> halt HTTP.status302

-- | Issue an HTTP 301 (Moved Permantently) response,
-- with `location' as the destination.
redirectPermanently :: ByteString -> Handler s m a
redirectPermanently location =
    addResponseHeader ("Location", location) >> halt HTTP.status301
