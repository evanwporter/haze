{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text
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
            [ goldenParserTest
                parseComment
                "simple comment"
                "test/golden/comment-simple.golden"
                "$comment hello world $end"
            , goldenParserTest
                parseComment
                "multiline comment"
                "test/golden/comment-multiline.golden"
                "$comment hello\nthis is another line\n$end"
            , goldenParserTest
                parseComment
                "empty comment"
                "test/golden/comment-empty.golden"
                "$comment $end"
            ]
        , testGroup
            "parseDate"
            [ goldenParserTest
                parseDate
                "simple date"
                "test/golden/date-simple.golden"
                "$date September 26, 2026 $end"
            , goldenParserTest
                parseDate
                "multiline date"
                "test/golden/date-multiline.golden"
                "$date\nSeptember 26, 2026\n$end"
            , goldenParserTest
                parseDate
                "empty date"
                "test/golden/date-empty.golden"
                "$date $end"
            ]
        , testGroup
            "parseVar"
            [ testCase "parses plain identifier variable" $
                parseOnly parseVar (T.pack "$var wire 8 # data $end")
                    @?= Right
                        ( Var
                            Wire
                            (Size 8)
                            (IdentifierCode "#")
                            (Identifier "data")
                        )
            , testCase "parses bit select variable" $
                parseOnly parseVar (T.pack "$var wire 1 # data[3] $end")
                    @?= Right
                        ( Var
                            Wire
                            (Size 1)
                            (IdentifierCode "#")
                            (BitSelect "data" (Index 3))
                        )
            , testCase "parses range select variable" $
                parseOnly parseVar (T.pack "$var wire 8 # data[7:0] $end")
                    @?= Right
                        ( Var
                            Wire
                            (Size 8)
                            (IdentifierCode "#")
                            (RangeSelect "data" (Index 7) (Index 0))
                        )
            ]
        ]

goldenParserTest ::
    (Show a) =>
    Parser a ->
    TestName ->
    FilePath ->
    T.Text ->
    TestTree
goldenParserTest parser name goldenFile input =
    goldenVsString name goldenFile $
        pure . BL.pack . show $
            parseOnly parser input
