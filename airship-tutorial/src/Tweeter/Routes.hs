{-# LANGUAGE OverloadedStrings #-}
module Tweeter.Routes (
    routes
  , errors
) where

import Airship
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import           Tweeter.Data
import           Control.Monad.Trans.State (StateT)
import           Tweeter.TweetResource
import           Tweeter.TweetsResource
import           Tweeter.Db

routes :: TweetDb -> Resource (StateT TweetState IO) -> RoutingSpec (StateT TweetState IO) ()
routes db static = do
  "tweets" #> tweetsResource db
  "tweets" </> var "tweet_id" #> tweetResource db
  star #> static

errors :: Monad m => M.Map HTTP.Status [(HTTP.MediaType, m ResponseBody)]
errors =
  let response404 = escapedResponse "<html><head></head><body><h1>404 Not Found</h1></body></html>"
  in M.singleton HTTP.status404 [("text/html", return response404)]
