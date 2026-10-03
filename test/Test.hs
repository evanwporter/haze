{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ParserTest
import PipelineTest
import Test.Tasty
import TransitionsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ parserTests,
      transitionsTests,
      pipelineTests
    ]

