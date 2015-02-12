{-# LANGUAGE RankNTypes #-}

module Airship.Headers
    ( addResponseHeader
    , modifyResponseHeaders
    ) where

import Airship.Types (Handler, ResponseState(..))
import Control.Monad.State.Class (modify)
import Network.HTTP.Types (ResponseHeaders, Header)

modifyResponseHeaders :: (ResponseHeaders -> ResponseHeaders) -> Handler s m ()
modifyResponseHeaders f = modify updateHeaders
    where updateHeaders rs@ResponseState{stateHeaders = h} = rs { stateHeaders = f h }

addResponseHeader :: Header -> Handler s m ()
addResponseHeader h = modifyResponseHeaders (h :)
