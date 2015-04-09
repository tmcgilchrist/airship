{-# LANGUAGE CPP #-}
module Airship.Internal.Date
    ( parseRfc1123Date ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif

import           Data.ByteString (ByteString)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
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
