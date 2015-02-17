module Airship.Date
    ( parseRfc1123Date ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Date (HTTPDate, parseHTTPDate)

httpDateToUtc :: HTTPDate -> UTCTime
httpDateToUtc = undefined

parseRfc1123Date :: ByteString -> Maybe UTCTime
parseRfc1123Date b = httpDateToUtc <$> parseHTTPDate b
