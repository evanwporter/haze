module Main (main) where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Parser Tests"
    [ testGroup
        "parseValue"
        [ testCase "parses '0'" $
            parseOnly parseValue (T.pack "0") @?= Right V0,
          testCase "parses '1'" $
            parseOnly parseValue (T.pack "1") @?= Right V1,
          testCase "parses 'x'" $
            parseOnly parseValue (T.pack "x") @?= Right Vx,
          testCase "parses 'X'" $
            parseOnly parseValue (T.pack "X") @?= Right VX,
          testCase "parses 'z'" $
            parseOnly parseValue (T.pack "z") @?= Right Vz,
          testCase "parses 'Z'" $
            parseOnly parseValue (T.pack "Z") @?= Right VZ
        ]
    ]
