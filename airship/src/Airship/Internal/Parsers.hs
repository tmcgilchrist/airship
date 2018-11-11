{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Airship.Internal.Parsers
    ( parseEtag
    , parseEtagList
    ) where

import Prelude hiding (takeWhile)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<|>), (*>), (<*))
#else
import Control.Applicative ((<|>))
#endif
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, sepBy', char,
                                         string, takeWhile,
                                         takeWhile1, inClass, endOfInput)
import Data.ByteString (ByteString)

import Airship.Types (ETag(..))

comma :: Parser Char
comma = char ','

doubleQuote :: Char
doubleQuote = '"'

insideQuotes :: Parser a -> Parser a
insideQuotes a = char doubleQuote *> a <* char doubleQuote

optionalWhitespace :: Parser ByteString
optionalWhitespace = takeWhile (inClass " \t")

insideWhitespace :: Parser a -> Parser a
insideWhitespace a = optionalWhitespace *> a <* optionalWhitespace

weakETag :: Parser ETag
weakETag = Weak <$> (string "W/" *> insideQuotes rest)
    where rest = takeWhile1 (/= doubleQuote)

strongETag :: Parser ETag
strongETag = insideQuotes strong
    where strong = Strong <$> takeWhile1 (/= doubleQuote)

eTag :: Parser ETag
eTag = insideWhitespace (weakETag <|> strongETag)

parseEtag :: ByteString -> Maybe ETag
parseEtag input = either (const Nothing) Just (parseOnly eTagToEnd input)
    where eTagToEnd = eTag <* endOfInput

-- | Parse a list of Etags, returning an empty list if parsing fails
parseEtagList :: ByteString -> [ETag]
parseEtagList input = either (const []) id parseResult
    where parseResult = parseOnly eTagList input
          eTagList = (eTag `sepBy'` comma) <* endOfInput
