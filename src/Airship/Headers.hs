{-# LANGUAGE RankNTypes #-}

module Airship.Headers
    ( addResponseHeader
    , modifyResponseHeaders
    , setResponseHeader
    ) where

import Airship.Types (Webmachine, ResponseState(..))
import Data.Function (on)
import Data.List as DL
import Control.Monad.State.Class (modify)
import Network.HTTP.Types (ResponseHeaders, Header)

-- | Applies the given function to the 'ResponseHeaders' present in this handlers 'ResponseState'.
modifyResponseHeaders :: Monad m => (ResponseHeaders -> ResponseHeaders) -> Webmachine m ()
modifyResponseHeaders f = modify updateHeaders
    where updateHeaders rs@ResponseState{stateHeaders = h} = rs { stateHeaders = f h }

-- | Adds a given 'Header' to this handler's 'ResponseState'.
addResponseHeader :: Monad m => Header -> Webmachine m ()
addResponseHeader h = modifyResponseHeaders (h :)

-- | Set a given 'Header' to this handler's 'ResponseState'.
setResponseHeader :: Monad m => Header -> Webmachine m ()
setResponseHeader s = modifyResponseHeaders $ (s :) . DL.deleteBy (on (==) fst) s
