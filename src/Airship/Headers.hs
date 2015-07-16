{-# LANGUAGE RankNTypes #-}

module Airship.Headers
    ( addResponseHeader
    , modifyResponseHeaders
    , replaceResponseHeader
    ) where

import           Airship.Types             (Handler, ResponseState (..))
import           Control.Monad.State.Class (modify)
import qualified Data.HashMap.Lazy         as HM
import           Network.HTTP.Types        (Header, ResponseHeaders)

-- | Applies the given function to the 'ResponseHeaders' present in this 'Handler''s 'ResponseState'.
modifyResponseHeaders :: (ResponseHeaders -> ResponseHeaders) -> Handler s m ()
modifyResponseHeaders f = modify updateHeaders
    where updateHeaders rs@ResponseState{stateHeaders = h} = rs { stateHeaders = f h }

-- | Adds a given 'Header' to this handler's 'ResponseState'.
addResponseHeader :: Header -> Handler s m ()
addResponseHeader h = modifyResponseHeaders (h :)

-- | Replaces the value for a given 'Header' in this handler's
-- 'ResponseState' or adds it if it does not already exist.
replaceResponseHeader :: Header -> Handler s m ()
replaceResponseHeader (k, v) = do
  let replaceHeader = \x -> HM.toList $ HM.insert k v (HM.fromList x)
  modifyResponseHeaders replaceHeader
