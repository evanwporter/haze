{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Parser
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import qualified Text.Pretty.Simple as PS
import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Parser Tests"
        [ declarationTests
        , simulationTests
        , vcdTests
        ]

declarationTests :: TestTree
declarationTests =
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
                parseOnly parseTimeScale (T.pack "$timescale 1ns $end")
                    @?= Right (TimeScale T1 NanoSeconds)
            , testCase "parses 10 ps timescale" $
                parseOnly parseTimeScale (T.pack "$timescale 10ps $end")
                    @?= Right (TimeScale T10 PicoSeconds)
            , testCase "parses 100 fs timescale" $
                parseOnly parseTimeScale (T.pack "$timescale 100fs $end")
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

simulationTests :: TestTree
simulationTests =
    testGroup
        "Simulation Parsers"
        [ testGroup
            "parseScalarValueChange"
            [ testCase "parses scalar 0" $
                parseOnly parseScalarValueChange "0!"
                    @?= Right
                        (ScalarValueChange V0 (IdentifierCode "!"))
            , testCase "parses scalar 1" $
                parseOnly parseScalarValueChange "1$"
                    @?= Right
                        (ScalarValueChange V1 (IdentifierCode "$"))
            , testCase "parses scalar x" $
                parseOnly parseScalarValueChange "x%"
                    @?= Right
                        (ScalarValueChange Vx (IdentifierCode "%"))
            , testCase "parses scalar z" $
                parseOnly parseScalarValueChange "z&"
                    @?= Right
                        (ScalarValueChange Vz (IdentifierCode "&"))
            ]
        , testGroup
            "parseVectorValueChange"
            [ testCase "parses lowercase binary vector" $
                parseOnly parseVectorValueChange "b1010 #"
                    @?= Right
                        (BinaryLower "1010" (IdentifierCode "#"))
            , testCase "parses uppercase binary vector" $
                parseOnly parseVectorValueChange "B11110000 !"
                    @?= Right
                        (BinaryUpper "11110000" (IdentifierCode "!"))
            , testCase "parses binary vector containing x" $
                parseOnly parseVectorValueChange "bxxxxxxxx #"
                    @?= Right
                        (BinaryLower "xxxxxxxx" (IdentifierCode "#"))
            , testCase "parses lowercase real value" $
                parseOnly parseVectorValueChange "r3.14 $"
                    @?= Right
                        (RealLower 3.14 (IdentifierCode "$"))
            , testCase "parses uppercase real value" $
                parseOnly parseVectorValueChange "R-2.5 %"
                    @?= Right
                        (RealUpper (-2.5) (IdentifierCode "%"))
            ]
        , testGroup
            "parseValueChange"
            [ testCase "dispatches scalar value change" $
                parseOnly parseValueChange "1!"
                    @?= Right
                        ( ScalarChange
                            (ScalarValueChange V1 (IdentifierCode "!"))
                        )
            , testCase "dispatches binary vector value change" $
                parseOnly parseValueChange "b10000001 #"
                    @?= Right
                        ( VectorChange
                            (BinaryLower "10000001" (IdentifierCode "#"))
                        )
            , testCase "dispatches real value change" $
                parseOnly parseValueChange "r1.5 $"
                    @?= Right
                        ( VectorChange
                            (RealLower 1.5 (IdentifierCode "$"))
                        )
            ]
        , testGroup
            "parseSimulationTime"
            [ testCase "parses time zero" $
                parseOnly parseSimulationTime "#0"
                    @?= Right
                        (SimTime (SimulationTime 0))
            , testCase "parses nonzero time" $
                parseOnly parseSimulationTime "#2211"
                    @?= Right
                        (SimTime (SimulationTime 2211))
            ]
        , testGroup
            "parseSimulationKeyword"
            [ testCase "parses dumpvars" $
                parseOnly
                    parseSimulationKeyword
                    "$dumpvars bxxxxxxxx # x$ 0% 1& $end"
                    @?= Right
                        ( DumpVars
                            [ VectorChange
                                (BinaryLower "xxxxxxxx" (IdentifierCode "#"))
                            , ScalarChange
                                (ScalarValueChange Vx (IdentifierCode "$"))
                            , ScalarChange
                                (ScalarValueChange V0 (IdentifierCode "%"))
                            , ScalarChange
                                (ScalarValueChange V1 (IdentifierCode "&"))
                            ]
                        )
            , testCase "parses dumpoff" $
                parseOnly
                    parseSimulationKeyword
                    "$dumpoff 0! x$ $end"
                    @?= Right
                        ( DumpOff
                            [ ScalarChange
                                (ScalarValueChange V0 (IdentifierCode "!"))
                            , ScalarChange
                                (ScalarValueChange Vx (IdentifierCode "$"))
                            ]
                        )
            , testCase "parses dumpon" $
                parseOnly
                    parseSimulationKeyword
                    "$dumpon 1! $end"
                    @?= Right
                        ( DumpOn
                            [ ScalarChange
                                (ScalarValueChange V1 (IdentifierCode "!"))
                            ]
                        )
            , testCase "parses dumpall" $
                parseOnly
                    parseSimulationKeyword
                    "$dumpall b1010 # $end"
                    @?= Right
                        ( DumpAll
                            [ VectorChange
                                (BinaryLower "1010" (IdentifierCode "#"))
                            ]
                        )
            ]
        , testGroup
            "parseSimulationCommand"
            [ testCase "dispatches simulation time" $
                parseOnly parseSimulationCommand "#2303"
                    @?= Right
                        (SimTime (SimulationTime 2303))
            , testCase "dispatches scalar value change" $
                parseOnly parseSimulationCommand "0'"
                    @?= Right
                        ( SimValueChange
                            ( ScalarChange
                                (ScalarValueChange V0 (IdentifierCode "'"))
                            )
                        )
            , testCase "dispatches binary vector change" $
                parseOnly parseSimulationCommand "b10000001 #"
                    @?= Right
                        ( SimValueChange
                            ( VectorChange
                                (BinaryLower "10000001" (IdentifierCode "#"))
                            )
                        )
            , testCase "dispatches dumpvars" $
                parseOnly
                    parseSimulationCommand
                    "$dumpvars 0! 1$ $end"
                    @?= Right
                        ( DumpVars
                            [ ScalarChange
                                (ScalarValueChange V0 (IdentifierCode "!"))
                            , ScalarChange
                                (ScalarValueChange V1 (IdentifierCode "$"))
                            ]
                        )
            ]
        ]

vcdTests :: TestTree
vcdTests =
    testGroup
        "VCD Files"
        [ goldenVCDTest
            parseVCD
            "parses sample VCD"
            "test/vcd/sample.vcd"
            "test/golden/sample-vcd.golden"
        , goldenVCDTest
            parseVCD
            "parses wikipedia VCD"
            "test/vcd/wikipedia.vcd"
            "test/golden/wikipedia-vcd.golden"
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

goldenVCDTest ::
    (Show a) =>
    Parser a ->
    TestName ->
    FilePath ->
    FilePath ->
    TestTree
goldenVCDTest parser name inputFile goldenFile =
    goldenVsString name goldenFile $ do
        input <- TIO.readFile inputFile

        pure . BL.pack $
            case parseOnly parser input of
                Left err ->
                    "Left " ++ show err
                Right result ->
                    TL.unpack $ PS.pShowNoColor result
