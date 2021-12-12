{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Tweeter.Data (
    Tweet(..)
  , TweetId(..)
  , KeyedTweet(..)
  , TweetState (..)

  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Data.Time

newtype TweetState = TweetState {
    tweet :: Maybe KeyedTweet
  }

newtype TweetId = TweetId {
    unTweetId :: Text
  } deriving (Eq, Show, Ord)

data Tweet = Tweet {
    tweetMsg :: Text
  , tweetAvatar :: Text
  , tweetTime :: UTCTime
  } deriving (Eq, Show)

-- Sort Tweets by TweetTime
instance Ord Tweet where
  compare (Tweet _ _ t1) (Tweet _ _ t2) = compare t1 t2

data KeyedTweet = KeyedTweet {
    unKey :: TweetId
  , unValue  :: Tweet
  } deriving (Eq, Show, Ord)

-- Explicit To/FromJSON instances allow us to control the structure of the json generated
-- Automatic deriving of these instances can lead to unstable serialisation
--
instance ToJSON KeyedTweet where
  toJSON (KeyedTweet (TweetId tid) (Tweet msg avatar time)) =
    object [ "message"   .= msg
           , "avatar"    .= avatar
           , "time"      .= time
           , "tweet_id"  .= tid]

instance FromJSON KeyedTweet where
  parseJSON (Object o) =
    KeyedTweet <$> (TweetId <$> o .: "tweet_id")
               <*> (Tweet <$> o .: "message"
                          <*> o .: "avatar"
                          <*> o .: "time")
  parseJSON x = typeMismatch "KeyedTweet" x

instance ToJSON Tweet where
  toJSON (Tweet msg avatar time) =
    object [ "message" .= msg
           , "avatar"  .= avatar
           , "time"    .= time]

instance FromJSON Tweet where
  parseJSON (Object o) =
    Tweet <$> o .: "message"
          <*> o .: "avatar"
          <*> o .: "time"
  parseJSON x = typeMismatch "Tweet" x
