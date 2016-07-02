module Main where

import Criterion
import Criterion.Main
import Data.ByteString
import Data.Time
import Data.Time.Clock.POSIX
import Airship.Internal.Date

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

main :: IO ()
main = defaultMain [
    bgroup "Airship.Internal.Date" [
        bench "utcTimeToRfc1123" $ nf utcTimeToRfc1123 epoch
    ]]
