{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Versions where

import           Airship
import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           Control.Applicative                ((<$>))
import           Control.Concurrent.MVar
import           Control.Monad.State                hiding (State)

import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Text as T

import qualified Network.HTTP.Types                 as HTTP
import           Network.Wai.Handler.Warp           (defaultSettings,
                                                     runSettings, setHost,
                                                     setPort)
import qualified Data.Map as M
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import           Data.Aeson
import qualified System.IO as IO

-- ***************************************************************************
-- Basic Data Store
-- ***************************************************************************
type Db = MVar (M.Map DirigibleId Dirigible)

createDb :: IO Db
createDb = do
  s <- newMVar M.empty
  return s

insert :: Db -> Dirigible -> IO DirigibleId
insert db t = do
  uuid <- liftIO U.nextRandom >>= return . DirigibleId . T.pack . U.toString
  modifyMVar_ db $ \m ->
    return $ M.insert uuid t m
  return uuid

list :: Db -> IO [(DirigibleId,  Dirigible)]
list t = modifyMVar t $ \m ->
  return (m, M.assocs m)

-- ***************************************************************************
-- Data Types
-- ***************************************************************************
newtype DirigibleId = DirigibleId { unDirigibleId :: T.Text } deriving (Eq, Show, Ord)

data Dirigible = Dirigible {
    dName :: T.Text
  , dEngines :: Int
  , dCaptain :: Maybe T.Text
  , dLiftCapacity :: Integer
  } deriving (Eq, Show, Ord)

newtype DirigibleV1 = DirigibleV1 Dirigible
newtype DirigibleV2 = DirigibleV2 Dirigible

instance ToJSON DirigibleV1  where
  toJSON (DirigibleV1 (Dirigible n e c l)) =
    object [ "name" .= n
           , "engines" .= e
           , "captain" .= c
           , "lift_capacity" .= l]

instance ToJSON DirigibleV2  where
  toJSON (DirigibleV2 (Dirigible n e _c l)) =
    object [ "name" .= n
           , "engines" .= e
           , "lift_capacity" .= l]

instance ToJSON DirigibleId where
  toJSON (DirigibleId i) =
    object [ "id" .= i ]

jsonResponseV1 :: Db -> Webmachine IO ResponseBody
jsonResponseV1 db = do
  t <- lift $ list db
  return $ encodeResponse ((\(x,y) -> (x, DirigibleV1 y)) <$> t)

jsonResponseV2 :: Db -> Webmachine IO ResponseBody
jsonResponseV2 db = do
  t <- lift $ list db
  return $ encodeResponse ((\(x,y) -> (x, DirigibleV2 y)) <$> t)

dirigibleResource :: Db -> Resource IO
dirigibleResource db = defaultResource {
        allowedMethods = return [ HTTP.methodGet ]
      , contentTypesProvided = return [ ("application/v1+json", jsonResponseV1 db)
                                      , ("application/v2+json", jsonResponseV2 db)]
    }

encodeResponse :: ToJSON a => a -> ResponseBody
encodeResponse = ResponseBuilder . fromByteString . BSL.toStrict . encode

routes :: Db -> RoutingSpec IO ()
routes db = do
      "dirigible" @> dirigibleResource db

main :: IO ()
main = do
    let port = 3000
        host = "127.0.0.1"
        settings = setPort port (setHost host defaultSettings)
        response404 = escapedResponse "<html><head></head><body><h1>404 Not Found</h1></body></html>"
        resource404 = defaultResource { resourceExists = return False
                                      , contentTypesProvided = return
                                            [ ( "text/html"
                                              , return response404
                                              )
                                            ]
                                      }
    db <- createDb
    insertDirigibles db
    IO.putStrLn "Listening on port 3000"
    runSettings settings (resourceToWai defaultAirshipConfig (routes db) resource404)

insertDirigibles :: Db -> IO ()
insertDirigibles db = do
  let a = [ Dirigible "La France" 1 (Just "Charles Renard") 100
          , Dirigible "Luftschiff Zeppelin LZ1" 2 (Just "Count von Zeppelin") 12428 ]
  forM_ a $ insert db


-- curl -X GET http://localhost:3000/dirigible -v -H "Accept: application/v1+json"
-- gives V1 of the api
--
-- curl -X GET http://localhost:3000/dirigible -v -H "Accept: application/v2+json"
-- gives V2 of the api
--
-- curl -X GET http://localhost:3000/dirigible -v
-- gives Accept: * / * so it matches V1
