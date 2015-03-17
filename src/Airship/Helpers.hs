{-# LANGUAGE RankNTypes #-}

module Airship.Helpers where

import           Control.Applicative
import           Data.Maybe
import           Network.HTTP.Media
import qualified Network.HTTP.Types as HTTP
import           Network.Wai

import           Airship.Types

-- | Returns @True@ if the request's Content-Type header is one of the
-- provided media types. If the Content-Type header is not present,
-- this function will return True. (TODO: does that make sense?)
contentTypeMatches :: [MediaType] -> Handler s m Bool
contentTypeMatches validTypes = do
    headers <- requestHeaders <$> request
    let cType = lookup HTTP.hContentType headers
    return $ case cType of
        Nothing -> True
        Just t  -> isJust $ matchAccept validTypes t
