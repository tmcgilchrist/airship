{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Airship.Resource2.Route
  ( RoutingSpec
  , runRouter
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Identity
import           Control.Monad.Writer (Writer, WriterT (..), execWriter)
import           Control.Monad.Writer.Class (MonadWriter)

import           Airship.Resource2.Data
import           Airship.Route


runRouter :: RoutingSpec m a -> [(Route, Resource m)]
runRouter routes = execWriter (getRouter routes)

-- | Represents a fully-specified set of routes that map paths (represented as 'Route's) to 'Resource's. 'RoutingSpec's are declared with do-notation, to wit:
--
-- @
--    myRoutes :: RoutingSpec IO ()
--    myRoutes = do
--      root                                 #> myRootResource
--      "blog" '</>' var "date" '</>' var "post" #> blogPostResource
--      "about"                              #> aboutResource
--      "anything" '</>' star                  #> wildcardResource
-- @
--
newtype RoutingSpec m a = RoutingSpec { getRouter :: Writer [(Route, Resource m)] a }
     deriving (Functor, Applicative, Monad, MonadWriter [(Route, Resource m)])
