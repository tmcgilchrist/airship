{-# LANGUAGE OverloadedStrings #-}
import Airship
import Airship.Resource.Static ( staticResource, StaticOptions (..) )
import Control.Monad ( forM_ )
import Data.Time ( getCurrentTime )
import Network.Wai.Handler.Warp ( runSettings
                                , defaultSettings
                                , setPort
                                , setHost )

import System.IO ( BufferMode(..), stdout, stderr, hSetBuffering )
import Tweeter ( TweetDb (..), Tweet (..), TweetState(..) )
import qualified Tweeter as T
import           Control.Monad.Trans.State ( StateT, evalStateT, get, put )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  static <- staticResource Cache "assets"

  db <- T.init
  insertTweets db

  let port = 3000
      host = "0.0.0.0"
      settings = setPort port (setHost host defaultSettings)
      routes' = T.routes db static

  let s = TweetState Nothing
  putStrLn "Listening on port 3000"
  runSettings settings (resourceToWaiT defaultAirshipConfig (const $ flip evalStateT s) routes' T.errors)

insertTweets :: TweetDb -> IO ()
insertTweets db = do
  now <- getCurrentTime
  now' <- getCurrentTime
  now'' <- getCurrentTime
  let tweets = [ Tweet "Pawns." "http://upload.wikimedia.org/wikipedia/en/thumb/f/f4/The_Wire_Jimmy_McNulty.jpg/250px-The_Wire_Jimmy_McNulty.jpg" now
               , Tweet "A man's gotta have a code." "http://upload.wikimedia.org/wikipedia/en/thumb/1/15/The_Wire_Bunk.jpg/250px-The_Wire_Bunk.jpg" now'
               , Tweet "You boys have a taste?" "http://upload.wikimedia.org/wikipedia/en/thumb/f/f4/The_Wire_Jimmy_McNulty.jpg/250px-The_Wire_Jimmy_McNulty.jpg" now''
               ]
  forM_ tweets (\x -> T.insert db x)
