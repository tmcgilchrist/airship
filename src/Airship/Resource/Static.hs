{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Airship.Resource.Static
    ( FileInfo(..)
    , StaticOptions(..)
    , staticResource
    , allFilesAtRoot
    , epochToUTCTime
    , directoryTree
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif

import           Airship.Headers (addResponseHeader)
import           Airship.Types ( ETag(Strong)
                               , ResponseBody(ResponseFile)
                               , Handler
                               , dispatchPath
                               , halt
                               )
import           Airship.Resource (Resource(..), defaultResource)


import           Control.Monad (foldM, when)
import qualified Crypto.Hash.MD5 as MD5
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64URL
import           Data.ByteString.Char8 (pack, split)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Trie as Trie
import           Network.HTTP.Media ((//))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Mime as Mime
import qualified System.Directory as D
import           System.FilePath (takeFileName)
import qualified System.Posix.Files as Files
import           System.IO (IOMode(ReadMode), withBinaryFile)
import           System.Posix.Types (EpochTime)


data FileTree = FileTree { tree :: Trie.Trie FileInfo
                         , root :: T.Text
                         }

data FileInfo = FileInfo
    { _path          :: FilePath
    , _size          :: Integer
    , _lastModified  :: UTCTime
    , _etag          :: ETag
    } deriving (Show, Eq, Ord)

data StaticOptions = Cache | NoCache deriving (Eq)

epochToUTCTime :: EpochTime -> UTCTime
epochToUTCTime = posixSecondsToUTCTime . realToFrac

fileETag :: FilePath -> IO ETag
fileETag p = withBinaryFile p ReadMode makeEtag
    where makeEtag h = do
            let ctx = MD5.init
            res <- go ctx h
            return (Strong (BS.take 22 (Base64URL.encode (MD5.finalize res))))
          go ctx h = do
                bs <- BS.hGetSome h 1024
                if BS.null bs
                    then return ctx
                    else return (MD5.update ctx bs)


filteredDirectory :: FilePath -> IO [FilePath]
filteredDirectory p = filter (not . (`elem` [".", ".."])) <$> D.getDirectoryContents p

allFilesAtRoot :: FilePath -> IO [FilePath]
allFilesAtRoot p = filteredDirectory p >>= foldM folder []
    where folder :: [FilePath] -> FilePath -> IO [FilePath]
          folder acc f = do
            let fullPath = p <> "/" <> f
            exists <- D.doesDirectoryExist fullPath
            if exists
                then do
                    more <- allFilesAtRoot (p <> "/" <> f)
                    return (more ++ acc)
                else return (fullPath : acc)

regularFileStatus :: [FilePath] -> IO [(FilePath, Files.FileStatus)]
regularFileStatus fs = filter (Files.isRegularFile . snd) <$>
                        mapM (\f -> (,) f <$> Files.getFileStatus f) fs

fileInfos :: [(FilePath, Files.FileStatus, ETag)] -> [(ByteString, FileInfo)]
fileInfos = map (\(p, s, e) -> (pack p, statusToInfo p s e))

statusToInfo :: FilePath -> Files.FileStatus -> ETag -> FileInfo
statusToInfo p i e = FileInfo { _path = p
                              , _size = fromIntegral (Files.fileSize i)
                              , _lastModified = epochToUTCTime (Files.modificationTime i)
                              , _etag = e
                              }

directoryTree :: FilePath -> IO FileTree
directoryTree f = do
    regularFiles <- allFilesAtRoot f >>= regularFileStatus
    etags <- mapM (fileETag . fst) regularFiles
    let infos = fileInfos (zipWith (\(a,b) c -> (a,b,c)) regularFiles etags)
    return (FileTree (Trie.fromList infos) (T.pack f))

staticResource :: StaticOptions -> FilePath -> IO (Resource m)
staticResource options p = staticResource' options <$> directoryTree p

staticResource' :: StaticOptions -> FileTree -> Resource m
staticResource' options FileTree{..} = defaultResource
    { allowedMethods = return [ HTTP.methodGet, HTTP.methodHead ]
    , resourceExists = getFileInfo >> return True
    , generateETag = if options == Cache
                        then Just . _etag <$> getFileInfo
                        else return Nothing
    , lastModified = if options == Cache
                        then Just . _lastModified <$> getFileInfo
                        else return Nothing
    , contentTypesProvided = do
        fInfo <- getFileInfo
        when (options == NoCache) addNoCacheHeaders
        let response = return (ResponseFile (_path fInfo) Nothing)
            fileName = T.pack (takeFileName (_path fInfo))
            fromExtension = Mime.defaultMimeLookup fileName
            (a:b:_tl) = split '/' fromExtension
            mediaType = a // b
        return [ (mediaType, response)
               , ("application/octet-stream", response)]
    }
    where getFileInfo :: Handler m FileInfo
          getFileInfo = do
            dispath <- dispatchPath
            let key = encodeUtf8 (T.intercalate "/" (root:dispath))
            let res = Trie.lookup key tree
            case res of
                (Just r) -> return r
                Nothing -> halt HTTP.status404

addNoCacheHeaders :: Handler m ()
addNoCacheHeaders = do
    addResponseHeader (HTTP.hCacheControl, "no-cache, no-store, must-revalidate")
    addResponseHeader ("Pragma", "no-cache")
    addResponseHeader ("Expires", "0")
