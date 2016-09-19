{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   Portions of this file are copyright (c) 2009,  IIJ Innovation Institute Inc.
   The utcTimeToRfc1123 function was extracted from http-date, with slight
   modifications to operate on UTCTime values.
-}

module Airship.Internal.Date
    ( parseRfc1123Date
    , utcTimeToRfc1123) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative         ((<$>))
#endif

import           Data.ByteString.Char8       (ByteString, pack)
import           Data.ByteString.Char8       ()
import           Data.ByteString.Internal
import           Data.Time.Calendar          (fromGregorian, toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.Clock             (UTCTime (..), secondsToDiffTime)
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format            (defaultTimeLocale, formatTime)
#else
-- get defaultTimeLocale from old-locale
import           Data.Time.Format            (formatTime)
import           System.Locale               (defaultTimeLocale)
#endif

import qualified Network.HTTP.Date           as HD

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
utcTimeToRfc1123 (UTCTime day offset) =
    unsafeCreate 29 $ \ptr -> do
        cpy3 ptr weekDays (3 * w)
        poke (ptr `plusPtr`  3) comma
        poke (ptr `plusPtr`  4) spc
        int2 (ptr `plusPtr`  5) d
        poke (ptr `plusPtr`  7) spc
        cpy3 (ptr `plusPtr`  8) months (3 * m)
        poke (ptr `plusPtr` 11) spc
        int4 (ptr `plusPtr` 12) y
        poke (ptr `plusPtr` 16) spc
        int2 (ptr `plusPtr` 17) h
        poke (ptr `plusPtr` 19) colon
        int2 (ptr `plusPtr` 20) n
        poke (ptr `plusPtr` 22) colon
        int2 (ptr `plusPtr` 23) s
        poke (ptr `plusPtr` 25) spc
        poke (ptr `plusPtr` 26) (71 :: Word8)
        poke (ptr `plusPtr` 27) (77 :: Word8)
        poke (ptr `plusPtr` 28) (84 :: Word8)
  where
    y = fromIntegral y'
    offset' = round offset
    h = offset' `mod` 3600
    n = offset' `mod` 60
    s = offset' - (h * 3600) - (n * 60)
    (y', m, d) = toGregorian day
    (_, _, w) = toWeekDate day
    cpy3 :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO ()
    cpy3 ptr p o = withForeignPtr p $ \fp ->
      memcpy ptr (fp `plusPtr` o) 3

----------------------------------------------------------------

int2 :: Ptr Word8 -> Int -> IO ()
int2 ptr n
  | n < 10 = do
      poke ptr zero
      poke (ptr `plusPtr` 1) (i2w8 n)
  | otherwise = do
      poke ptr               (i2w8 (n `quot` 10))
      poke (ptr `plusPtr` 1) (i2w8 (n `rem` 10))

int4 :: Ptr Word8 -> Int -> IO ()
int4 ptr n0 = do
    let (n1,x1) = n0 `quotRem` 10
        (n2,x2) = n1 `quotRem` 10
        (x4,x3) = n2 `quotRem` 10
    poke ptr               (i2w8 x4)
    poke (ptr `plusPtr` 1) (i2w8 x3)
    poke (ptr `plusPtr` 2) (i2w8 x2)
    poke (ptr `plusPtr` 3) (i2w8 x1)

i2w8 :: Int -> Word8
i2w8 n = fromIntegral n + zero

----------------------------------------------------------------

months :: ForeignPtr Word8
months = let (PS p _ _) = "___JanFebMarAprMayJunJulAugSepOctNovDec" in p

weekDays :: ForeignPtr Word8
weekDays = let (PS p _ _) = "___MonTueWedThuFriSatSun" in p

----------------------------------------------------------------

spc :: Word8
spc = 32

comma :: Word8
comma = 44

colon :: Word8
colon = 58

zero :: Word8
zero = 48
