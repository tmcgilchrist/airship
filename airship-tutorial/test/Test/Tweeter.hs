module Test.Tweeter (testMain, testCliMain) where
import           Control.Applicative
import           Control.Monad

import           System.Directory
import           System.Exit
import           System.IO
import           System.Process

import           Prelude

testMain :: [IO Bool] -> IO ()
testMain tests =
  sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

testCliMain :: [String] -> IO ()
testCliMain arguments =
  let ignore p = ".." == p || "." == p || "core" == p
      exec t = callProcess ("test/cli/" ++ t ++ "/run") arguments
   in sanity >> filter (not . ignore) <$> getDirectoryContents "test/cli/" >>= mapM_ exec

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
