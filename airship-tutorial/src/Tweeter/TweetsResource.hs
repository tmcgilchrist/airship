{-# LANGUAGE OverloadedStrings #-}
module Tweeter.TweetsResource (
    tweetsResource
  ) where

import           Airship
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.State (StateT)
import           Data.Aeson (encode, decode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import           Tweeter.Data (TweetState, TweetId (..))
import qualified Tweeter.Db as Db

tweetsResource :: MonadIO m => Db.TweetDb -> Resource (StateT TweetState m)
tweetsResource db = defaultResource
  { allowedMethods = pure [ HTTP.methodGet
                          , HTTP.methodHead
                          , HTTP.methodPost
                          ]
  , generateETag = etag db
  , forbidden = isForbidden
  , contentTypesProvided = pure [("application/json", jsonResponse db)]
  , allowMissingPost = pure True
  , processPost = createTweet db
  }

-- TODO Missing "text/event-stream" in contentTypesProvided
etag :: MonadIO m => Db.TweetDb -> Webmachine (StateT TweetState m) (Maybe ETag)
etag db = do
  t <- liftIO $ Db.list db
  pure . Just . Weak . md5sum . encode $ t

  where
    md5sum = T.encodeUtf8 . T.pack . show . md5

isForbidden :: Monad m => Webmachine m Bool
isForbidden = pure False

jsonResponse :: MonadIO m => Db.TweetDb -> Webmachine (StateT TweetState m) ResponseBody
jsonResponse db = do
  t <- liftIO $ Db.list db
  pure $ encodeResponse t

  where
    encodeResponse = ResponseBuilder . fromByteString . toStrict . encode

createTweet :: MonadIO m => Db.TweetDb -> Webmachine (StateT TweetState m) (PostResponse n)
createTweet db = do
  r <- liftIO . entireRequestBody =<< request
  t_id <- maybe (halt HTTP.status400) (liftIO . Db.insert db) =<< pure (decode r)
  pure . PostCreateRedirect . path $ t_id

  where
    path (TweetId t) = [t]
