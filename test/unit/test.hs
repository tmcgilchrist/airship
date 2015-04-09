{-# LANGUAGE OverloadedStrings #-}

module Main where

import Airship
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
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

type RequestState = State [ByteString]

bodyChunks :: [ByteString]
bodyChunks = ["one", "two", "three", "four", "five"]

nextBody :: RequestState ByteString
nextBody = do
    s <- get
    if null s
        then return BS.empty
        else do
            let (h:tl) = s
            put tl
            return h

bodyTest :: TestTree
bodyTest = testCase "entireRequestBody returns the body in the correct order" bodyTest'
    where bodyTest' = evalState state bodyChunks @?= "onetwothreefourfive"
          state :: RequestState LB.ByteString
          state = entireRequestBody req
          req = defRequest { requestBody = nextBody }
          -- for some reason this type signature seems to be necessary
          defRequest :: Request RequestState
          defRequest = defaultRequest
