{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ParserTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [parserTests]
