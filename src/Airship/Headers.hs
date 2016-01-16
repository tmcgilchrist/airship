{-# LANGUAGE RankNTypes #-}

module Airship.Headers
    ( addResponseHeader
    , modifyResponseHeaders
    ) where

import Airship.Types (Webmachine, ResponseState(..))
import Control.Monad.State.Class (modify)
import Network.HTTP.Types (ResponseHeaders, Header)

-- | Applies the given function to the 'ResponseHeaders' present in this handlers 'ResponseState'.
modifyResponseHeaders :: Monad m => (ResponseHeaders -> ResponseHeaders) -> Webmachine p m ()
modifyResponseHeaders f = modify updateHeaders
    where updateHeaders rs@ResponseState{stateHeaders = h} = rs { stateHeaders = f h }

-- | Adds a given 'Header' to this handler's 'ResponseState'.
addResponseHeader :: Monad m => Header -> Webmachine p m ()
addResponseHeader h = modifyResponseHeaders (h :)
