{-# LANGUAGE RankNTypes #-}

module Airship.Headers
    ( addResponseHeader
    , modifyResponseHeaders
    ) where

import Airship.Types (Handler, ResponseState(..))
import Control.Monad.State.Class (modify)
import Network.HTTP.Types (ResponseHeaders, Header)

-- | Applies the given function to the 'ResponseHeaders' present in this 'Handler''s 'ResponseState'.
modifyResponseHeaders :: (ResponseHeaders -> ResponseHeaders) -> Handler s m ()
modifyResponseHeaders f = modify updateHeaders
    where updateHeaders rs@ResponseState{stateHeaders = h} = rs { stateHeaders = f h }

-- | Adds a given 'Header' to this handler's 'ResponseState'.
addResponseHeader :: Header -> Handler s m ()
addResponseHeader h = modifyResponseHeaders (h :)
