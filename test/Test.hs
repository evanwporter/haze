{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Parser
import Test.Tasty
import Test.Tasty.Golden
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
                parseOnly parseValue (T.pack "0") @?= Right V0
            , testCase "parses '1'" $
                parseOnly parseValue (T.pack "1") @?= Right V1
            , testCase "parses 'x'" $
                parseOnly parseValue (T.pack "x") @?= Right Vx
            , testCase "parses 'X'" $
                parseOnly parseValue (T.pack "X") @?= Right VX
            , testCase "parses 'z'" $
                parseOnly parseValue (T.pack "z") @?= Right Vz
            , testCase "parses 'Z'" $
                parseOnly parseValue (T.pack "Z") @?= Right VZ
            ]
        , testGroup
            "parseTimeUnit"
            [ testCase "parses 'fs'" $
                parseOnly parseTimeUnit (T.pack "fs")
                    @?= Right FemtoSeconds
            , testCase "parses 'ps'" $
                parseOnly parseTimeUnit (T.pack "ps")
                    @?= Right PicoSeconds
            , testCase "parses 'ns'" $
                parseOnly parseTimeUnit (T.pack "ns")
                    @?= Right NanoSeconds
            , testCase "parses 'us'" $
                parseOnly parseTimeUnit (T.pack "us")
                    @?= Right MicroSeconds
            , testCase "parses 'ms'" $
                parseOnly parseTimeUnit (T.pack "ms")
                    @?= Right MilliSeconds
            , testCase "parses 's'" $
                parseOnly parseTimeUnit (T.pack "s")
                    @?= Right Seconds
            ]
        , testGroup
            "parseTimeNumber"
            [ testCase "parses '1'" $
                parseOnly parseTimeNumber (T.pack "1")
                    @?= Right T1
            , testCase "parses '10'" $
                parseOnly parseTimeNumber (T.pack "10")
                    @?= Right T10
            , testCase "parses '100'" $
                parseOnly parseTimeNumber (T.pack "100")
                    @?= Right T100
            ]
        , testGroup
            "parseTimeScale"
            [ testCase "parses 1 ns timescale" $
                parseOnly parseTimeScale (T.pack "$timescale ns1")
                    @?= Right (TimeScale T1 NanoSeconds)
            , testCase "parses 10 ps timescale" $
                parseOnly parseTimeScale (T.pack "$timescale ps10")
                    @?= Right (TimeScale T10 PicoSeconds)
            , testCase "parses 100 fs timescale" $
                parseOnly parseTimeScale (T.pack "$timescale fs100")
                    @?= Right (TimeScale T100 FemtoSeconds)
            ]
        , testGroup
            "parseComment"
            [ goldenCommentTest
                "simple comment"
                "test/golden/comment-simple.golden"
                "$comment hello world $end"
            , goldenCommentTest
                "multiline comment"
                "test/golden/comment-multiline.golden"
                "$comment hello\nthis is another line\n$end"
            , goldenCommentTest
                "empty comment"
                "test/golden/comment-empty.golden"
                "$comment $end"
            ]
        ]

goldenCommentTest :: TestName -> FilePath -> T.Text -> TestTree
goldenCommentTest name goldenFile input =
    goldenVsString name goldenFile $ do
        let result = parseOnly parseComment input
        pure . BL.pack $ show result
