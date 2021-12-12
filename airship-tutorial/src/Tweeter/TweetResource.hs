{-# LANGUAGE OverloadedStrings #-}
module Tweeter.TweetResource (
    tweetResource
  ) where

import           Airship
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Morph (lift)
import           Control.Monad.Trans.State (StateT, get, put)
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import           Tweeter.Data (TweetState (..), KeyedTweet (..), TweetId (..))
import qualified Tweeter.Db as Db

tweetResource :: MonadIO m => Db.TweetDb -> Resource (StateT TweetState m)
tweetResource db = defaultResource
  { contentTypesProvided = pure [("application/json", jsonResponse)]
  , forbidden = pure False -- TODO Check Security.isProtected
  , generateETag = etag
  , resourceExists = resourceExists' db
  }

resourceExists' :: MonadIO m => Db.TweetDb -> Webmachine (StateT TweetState m) Bool
resourceExists' db = do
  t_id <- TweetId <$> lookupParam "tweet_id"
  t <- liftIO $ Db.findById db t_id
  maybe (pure False)
    (\x -> do
        -- Put tweet into StateT
        lift . put . TweetState . Just . KeyedTweet t_id $ x
        pure True)
    t

jsonResponse :: MonadIO m => Webmachine (StateT TweetState m) ResponseBody
jsonResponse = do
  t <- lookupTweet
  pure . encodeResponse $ t

  where
    encodeResponse = ResponseBuilder . fromByteString . BSL.toStrict . encode

etag :: MonadIO m => Webmachine (StateT TweetState m) (Maybe ETag)
etag = do
  t <- lookupTweet
  pure . Just . Weak . md5sum . encode $ t

  where
    md5sum = T.encodeUtf8 . T.pack . show . md5

lookupTweet :: MonadIO m => Webmachine (StateT TweetState m) KeyedTweet
lookupTweet = (\t -> maybe (halt HTTP.status404) pure . tweet $ t) =<< lift get
