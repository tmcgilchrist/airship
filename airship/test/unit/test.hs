{-# LANGUAGE OverloadedStrings #-}
module Main where

import Airship (requestBody, entireRequestBody, defaultRequest)
import Control.Concurrent (newMVar, modifyMVar)
import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [examples]

examples :: TestTree
examples = testGroup "Examples" [exampleTests]

exampleTests :: TestTree
exampleTests = testGroup "ExampleTests"
  [ bodyTest ]

bodyChunks :: [ByteString]
bodyChunks = ["one", "two", "three", "four", "five"]

bodyChunksIO :: IO (IO ByteString)
bodyChunksIO = do
    v <- newMVar bodyChunks
    return $ modifyMVar v (\l -> return $ case l of { [] -> ([], ""); h : t -> (t, h) })

bodyTest :: TestTree
bodyTest = testCase "entireRequestBody returns the body in the correct order" $ do
    nextBody <- bodyChunksIO
    b <- entireRequestBody defaultRequest { requestBody = nextBody }
    b @?= "onetwothreefourfive"
