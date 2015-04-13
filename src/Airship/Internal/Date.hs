{-# LANGUAGE CPP #-}

module Airship.Internal.Date
    ( parseRfc1123Date
    , utcTimeToRfc1123) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format (formatTime, defaultTimeLocale)
#else
-- get defaultTimeLocale from old-locale
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)
#endif

import qualified Network.HTTP.Date as HD

httpDateToUtc :: HD.HTTPDate -> UTCTime
httpDateToUtc h = UTCTime days diffTime
    where days = fromGregorian (fromIntegral $ HD.hdYear h) (HD.hdMonth h) (HD.hdDay h)
          diffTime = secondsToDiffTime seconds
          seconds = fromIntegral $ hourS + minS + HD.hdSecond h
          hourS = HD.hdHour h * 60 * 60
          minS = HD.hdMinute h * 60

parseRfc1123Date :: ByteString -> Maybe UTCTime
parseRfc1123Date b = httpDateToUtc <$> HD.parseHTTPDate b

utcTimeToRfc1123 :: UTCTime -> ByteString
utcTimeToRfc1123 utc = pack $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" utc
