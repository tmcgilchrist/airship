{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tweeter.Db (
  -- * Data Types
   TweetDb

  -- * Functions
  , init
  , findById
  , insert
  , insert'
  , newTweetId
  , list
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as DL
import qualified Data.Map as M
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Prelude hiding (init)
import           Tweeter.Data (Tweet, TweetId(..), KeyedTweet(..))

-- MVar backed hashmap of tweet id's to tweets
type TweetDb = MVar (M.Map TweetId Tweet)

init :: MonadIO m => m TweetDb
init = do
  s <- liftIO $ newMVar M.empty
  pure s

findById :: MonadIO m => TweetDb -> TweetId -> m (Maybe Tweet)
findById t id' =
  liftIO . modifyMVar t $ \m ->
  pure (m, M.lookup id' m)

insert :: MonadIO m => TweetDb -> Tweet -> m TweetId
insert t t' = do
  uuid <- newTweetId
  insert' t uuid t'

insert' :: MonadIO m => TweetDb -> TweetId -> Tweet -> m TweetId
insert' db id' tweet = do
  liftIO . modifyMVar_ db $ \m ->
    pure $ M.insert id' tweet m
  pure id'

list :: MonadIO m => TweetDb -> m [KeyedTweet]
list t =
  liftIO . modifyMVar t $ \m ->
  pure (m, DL.sort . fmap (\(x,y) -> KeyedTweet x y) $ (M.assocs m))

newTweetId :: MonadIO m => m TweetId
newTweetId =
  liftIO nextRandom >>= pure . TweetId . toText
