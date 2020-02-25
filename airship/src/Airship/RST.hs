{-
  This file is copyright (c) 2009, the Snap Framework authors,
  and Patrick Thomson (for the Airship project).
  Used under the three-clause BSD license, the text of which may be
  found in the LICENSE file in the Airship root.
-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
  RST is like the RWST monad, but has no Writer instance, as Writer leaks space.
  This file is almost entirely lifted from the Snap framework's implementation.
-}

module Airship.RST
       ( RST (..)
       , evalRST
       , execRST
       , mapRST
       , withRST
       , failure
       ) where

import           Control.Applicative         (Alternative (..),
                                              Applicative (..))
import           Control.Category            ((.))
import           Control.Monad               (MonadPlus (..), ap)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Reader        (MonadReader (..))
import           Control.Monad.State.Class   (MonadState (..))
import           Control.Monad.Trans         (MonadIO (..), MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.Either
import           Prelude                     (Functor (..), Monad (..), seq,
                                              ($), ($!))


newtype RST r s e m a = RST { runRST :: r -> s -> m (Either e a, s) }


evalRST :: Monad m => RST r s e m a -> r -> s -> m (Either e a)
evalRST m r s = do
    (res, _) <- runRST m r s
    return $! res
{-# INLINE evalRST #-}


execRST :: Monad m => RST r s e m a -> r -> s -> m s
execRST m r s = do
    (_,!s') <- runRST m r s
    return $! s'
{-# INLINE execRST #-}


withRST :: Monad m => (r' -> r) -> RST r s e m a -> RST r' s e m a
withRST f m = RST $ \r' s -> runRST m (f r') s
{-# INLINE withRST #-}


instance (Monad m) => MonadReader r (RST r s e m) where
    ask = RST $ \r s -> return $! (Right r,s)
    local f m = RST $ \r s -> runRST m (f r) s

-- Terrible hack to work around the fact that Functor isn't a superclass
-- of Monad on GHC 7.8. TODO kill this when 7.8 support is dropped
#if __GLASGOW_HASKELL__ == 708
instance (Monad m) => Functor (RST r s e m) where
    fmap f m = RST $ \r s -> runRST m r s >>= helper where
      helper (a, s') = case a of
          (Left l) -> return $! (Left l, s')
          (Right r) -> return $! (Right $ f r, s')
#else
instance (Functor m) => Functor (RST r s e m) where
    fmap f m = RST $ \r s -> fmap (\(a,s') -> (fmap f a, s')) $ runRST m r s
#endif

instance Monad m => Applicative (RST r s e m) where
    pure = return
    (<*>) = ap


instance MonadPlus m => Alternative (RST r s e m) where
    empty = mzero
    (<|>) = mplus


instance (Monad m) => MonadState s (RST r s e m) where
    get   = RST $ \_ s -> return $! (Right s,s)
    put x = RST $ \_ _ -> return $! (Right (),x)
    state act = RST $ \_ s -> do
      let (res, !s') = act s
      return $! (Right res, s')


mapRST :: (m (Either e a, s) -> n (Either e b, s)) -> RST r s e m a -> RST r s e n b
mapRST f m = RST $ \r s -> f (runRST m r s)

rwsBind :: Monad m =>
           RST r s e m a
        -> (a -> RST r s e m b)
        -> RST r s e m b
rwsBind m f = RST go
  where
    go r !s = do
        (a, !s')  <- runRST m r s
        case a of
            Left e  -> return $! (Left e, s')
            Right a' ->  runRST (f a') r s'
{-# INLINE rwsBind #-}

instance (Monad m) => Monad (RST r s e m) where
    return a = RST $ \_ s -> return $! (Right a, s)
    (>>=)    = rwsBind
    -- fail msg = RST $ \_ _ -> fail msg

instance (MonadPlus m) => MonadPlus (RST r s e m) where
    mzero       = RST $ \_ _ -> mzero
    m `mplus` n = RST $ \r s -> runRST m r s `mplus` runRST n r s


instance (MonadIO m) => MonadIO (RST r s e m) where
    liftIO = lift . liftIO


instance MonadTrans (RST r s e) where
    lift m = RST $ \_ s -> do
        a <- m
        return $ s `seq` (Right a, s)


instance MonadBase b m => MonadBase b (RST r s e m) where
    liftBase = lift . liftBase


instance MonadBaseControl b m => MonadBaseControl b (RST r s e m) where
     type StM (RST r s e m) a = ComposeSt (RST r s e) m a
     liftBaseWith = defaultLiftBaseWith
     restoreM = defaultRestoreM
     {-# INLINE liftBaseWith #-}
     {-# INLINE restoreM #-}

instance MonadTransControl (RST r s e) where
    type StT (RST r s e) a = (Either e a, s)
    liftWith f = RST $ \r s -> do
        res <- f $ \(RST g) -> g r s
        return $! (Right res, s)
    restoreT k = RST $ \_ _ -> k
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

failure :: Monad m => e -> RST r s e m a
failure e = RST $ \_ s -> return $! (Left e, s)
