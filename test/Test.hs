{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ParserTest
import Test.Tasty
import TransitionsTest
import WaveformTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ parserTests,
      waveformTests,
      transitionsTests
    ]

