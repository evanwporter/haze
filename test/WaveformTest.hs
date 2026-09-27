{-# LANGUAGE OverloadedStrings #-}

module WaveformTest (waveformTests) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.Golden
import qualified Text.Pretty.Simple as PS

import Types
import Wave

waveformTests :: TestTree
waveformTests =
    testGroup
        "Waveform Tests"
        [ goldenBuildWaveformTest
        ]

goldenBuildWaveformTest :: TestTree
goldenBuildWaveformTest =
    goldenVsString
        "builds waveform from simulation commands"
        "test/golden/waveform-basic.golden"
        $ pure
            . BL.pack
            . TL.unpack
            . PS.pShowNoColor
            . fmap normalizeWaveform
        $ buildWaveform sampleSimulations

normalizeWaveform ::
    Waveform ->
    [(IdentifierCode, [(SimulationTime, WaveValue)])]
normalizeWaveform =
    sortOn fst . HM.toList

sampleSimulations :: [SimulationCommand]
sampleSimulations =
    [ DumpVars
        [ VectorChange
            (BinaryLower "xxxxxxxx" (IdentifierCode "#"))
        , ScalarChange
            (ScalarValueChange Vx (IdentifierCode "$"))
        ]
    , SimTime (SimulationTime 0)
    , SimValueChange
        ( VectorChange
            (BinaryLower "10000001" (IdentifierCode "#"))
        )
    , SimValueChange
        ( ScalarChange
            (ScalarValueChange V0 (IdentifierCode "$"))
        )
    , SimTime (SimulationTime 2296)
    , SimValueChange
        ( VectorChange
            (BinaryLower "0" (IdentifierCode "#"))
        )
    , SimValueChange
        ( ScalarChange
            (ScalarValueChange V1 (IdentifierCode "$"))
        )
    ]
