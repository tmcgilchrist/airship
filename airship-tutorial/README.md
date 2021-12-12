# Web Development with Airship for Haskell

## Airship (Webmachine)

## Building a REST Service

The running example will be tweeter, a short message broadcast service in no way similar to any existing things.
Tweeter provides an API to create, retrieve and delete these short messages. The exposed API will look like:

HTTP Verb | Route       | Content Type      | Status Code |
-----------------------------------------------------------
GET       | /tweet/id   | application/json  | 200         |
GET       | /tweets     | application/json  | 200         |
PUT       | /tweet/id   | application/json  | 201 (created) 200 (updated) |
POST      | /tweet      | application/json  | 201         |
DELETE    | /tweet/id   |                   | 204         |
GET       | /           | text/html         | 200         |
-----------------------------------------------------------

### Starting an Airship Application

Airship is built as an application on top of WAI.

```haskell

import Airship
import Airship.Resource.Static
import Network.Wai.Handler.Warp ( runSettings
                                , defaultSettings
                                , setPort
                                , setHost )
import System.IO
import Tweeter

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let port = 3000
      host = "0.0.0.0"
      settings = setPort port (setHost host defaultSettings)

  putStrLn "Listening on port 3000"
  runSettings settings (resourceToWai defaultAirshipConfig routes resource404)

routes :: RoutingSpec IO ()
routes = do
  star #> staticResource Cache "assets"

```

This will setup a wai application listening on port 3000 that serves static assets like CSS or JS, from the
`assets` directory.

### Configure Routes



### Adding a storage layer

Before we start accepting requests we're going to need a simple storage mechanism for our tweets. In the interests
of building the simplest possible thing, we're going to avoid using a real database and instead opt for a simple
hashmap with an MVar. The type of the store will look like this:

``` haskell
import qualified Data.Map as M
import           Control.Concurrent.MVar

type TweetDb = MVar (M.Map TweetId Tweet)

```
which is basically a mapping from a `TweetId` to an actual `Tweet`. Somewhat similar to how a key value or simple
datatbase table would work. Next we need to define those two types.

Starting with the `TweetId` we might be tempted to use an `Int` or `Text` (definately not `String`, as it's always
the wrong choice [Mark's Gist About this]()) but that's not really using the type system we have effectively.
Instead we'll use a `newtype` to introduce `TweetId` so we can't accidentially mix things of different type.

``` haskell
newtype TweetId = TweetId {
    unTweetId :: Text
  } deriving (Eq, Show, Ord)

```

The automatic deriving of typeclasses is required by `Data.Map`, which needs `Eq` and `Ord`, while the `Show` is
a convenience for us.

Next we introduce the `Tweet` type which includes a number of fields and also defines `Eq` and `Show`.
This time however we've decided to define our own `Ord` instance because we would like the Tweets to be ordered by
their `tweetTime` field rather than using the default derived instance of `Ord`. Aside if you're interested in how
derving works read this. [Link to how that works]()

``` haskell
data Tweet = Tweet {
    tweetMsg :: Text
  , tweetAvatar :: Text
  , tweetTime :: UTCTime
  } deriving (Eq, Show)

-- Sort Tweets by TweetTime
instance Ord Tweet where
  compare (Tweet _ _ t1) (Tweet _ _ t2) = compare t1 t2

```

Lastly we introduce a convenient type that wraps up a `TweetId` along with the `Tweet`, again following the idea
that everything should have a specific type. We could have just used `(TweetId, Tweet)` but the author believes
everything should be typed as specifically as possible.

``` haskell
data KeyedTweet = KeyedTweet {
    unKey :: TweetId
  , unValue  :: Tweet
  } deriving (Eq, Show, Ord)
```

The rest of the machinery for storage is defined in `src/Tweeter/Db.hs`, which is interesting to understand but
really we mainly care about the functions it provides. The types and names for the functions give a good idea of
what they're going to be used for.

``` haskell
init :: IO TweetDb
findById :: TweetDb -> TweetId -> IO (Maybe Tweet)
insert :: TweetDb -> Tweet -> IO TweetId
list :: TweetDb -> IO [KeyedTweet]
newTweetId :: IO TweetId
```
Now onto our first Airship resource.

### GET /tweet/id

The definition of the `Airship.Resource` type uses the `defaultResource` function that provides sensible values
for most fields in the `Resource` record. In this case it defaults to supporting `GET` so we only need to provide
our own implementation for `contentTypesProvided` and `resourceExists`.


``` haskell
tweetResource :: Db.TweetDb -> Resource (StateT TweetState IO)
tweetResource db = defaultResource
  { contentTypesProvided = return [("application/json", jsonResponse)]
  , resourceExists = resourceExists' db
  }

```

### Returning 404


### Responding in different formats


### PUT /tweet/id


### DELETE /tweet/id


### POST /tweet


### Testing

For testing we have a few approaches to look at:

 1. pure tests, eg are we serialising to json and back again properly
 2. individual resource tests, is a resource coherant within itself
 3. data store tests
 4. whole application tests, does the application with it's collection of routes behave as expected

That's a whole bunch of things to unpack, so lets get started on the simplest one first, pure tests.
