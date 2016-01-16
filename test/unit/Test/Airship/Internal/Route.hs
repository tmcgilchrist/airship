{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Airship.Internal.Route where

import           Airship.Internal.Route

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit ((@?=), testCase)


tests :: TestTree
tests =
  testGroup "Airship.Internal.Route" [
      testCase "matchRoutes root" testMatchRouteRoot
    , testCase "matchRoutes static" testMatchRouteStatic
    , testCase "matchRoutes var" testMatchRouteVar
    , testCase "matchRoutes star" testMatchRouteStar
    ] 

testMatchRouteRoot = do
  matchRoute' [] root @?= Just ((), [])
  matchRoute' ["a"] root @?= Nothing

testMatchRouteStatic = do
  matchRoute' ["a", "b", "c", "x"] (root </ "a" </ "b" </ "c" </ "d") @?= Nothing
  matchRoute' ["a", "b", "c", "d"] (root </ "a" </ "b" </ "c" </ "d") @?= Just ((), [])

testMatchRouteVar = do
  matchRoute' ["a", "b", "c", "x"] ((,) <$> "a" /> var </> var </ "d") @?= Nothing
  matchRoute' ["a", "b", "c"] ((,) <$> "a" /> var </> var </ "d") @?= Nothing
  matchRoute' ["a", "b", "c", "d"] ((,) <$> "a" /> var </> var </ "d") @?= Just (("b", "c"), ["d"])
  matchRoute' ["a", "b", "c", "d"] ((,) <$> "a" /> var </> "c" /> var) @?= Just (("b", "d"), ["c"])
  matchRoute' ["a", "b", "c", "d"] ((,) <$> "a" /> var </> ((,) <$> var </> var)) @?= Just (("b", ("c", "d")), [])

testMatchRouteStar = do
  matchRoute' ["a", "b", "c", "d"] star @?= Just ((), ["a", "b", "c", "d"])
  matchRoute' ["a", "b", "c", "d"] ("a" /> star) @?= Just ((), ["b", "c", "d"])
