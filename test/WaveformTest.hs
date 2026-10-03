{-# LANGUAGE OverloadedStrings #-}

module WaveformTest (waveformTests) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text.Lazy as TL
import Parser
import Test.Tasty
import Test.Tasty.Golden
import TestUtil
import qualified Text.Pretty.Simple as PS
import Types
import Waveform

waveformTests :: TestTree
waveformTests =
  testGroup
    "Waveform Tests"
    [ goldenBuildWaveformTest,
      waveformIntegrationTests
    ]

waveformIntegrationTests :: TestTree
waveformIntegrationTests =
  testGroup
    "Waveform Integration Tests"
    [ goldenFileTransformTest
        parseVCD
        buildNormalizedWaveform
        "builds waveform from sample VCD"
        "test/vcd/sample.vcd"
        "test/golden/sample-waveform.golden",
      goldenFileTransformTest
        parseVCD
        buildNormalizedWaveform
        "builds waveform from wikipedia VCD"
        "test/vcd/wikipedia.vcd"
        "test/golden/wikipedia-waveform.golden"
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
      . normalizeWaveform
    $ buildWaveform sampleSimulations

buildNormalizedWaveform ::
  ValueChangeDumpDefinitions ->
  Either String [(IdentifierCode, [(SimulationTime, WaveValue)])]
buildNormalizedWaveform vcd =
  Right . normalizeWaveform $
    buildWaveform (simulations vcd)

normalizeWaveform ::
  Waveform ->
  [(IdentifierCode, [(SimulationTime, WaveValue)])]
normalizeWaveform =
  sortOn fst . HM.toList

sampleSimulations :: [SimulationCommand]
sampleSimulations =
  [ DumpVars
      [ VectorChange
          (BinaryLower "xxxxxxxx" (IdentifierCode "#")),
        ScalarChange
          (ScalarValueChange Vx (IdentifierCode "$"))
      ],
    SimTime (SimulationTime 0),
    SimValueChange
      ( VectorChange
          (BinaryLower "10000001" (IdentifierCode "#"))
      ),
    SimValueChange
      ( ScalarChange
          (ScalarValueChange V0 (IdentifierCode "$"))
      ),
    SimTime (SimulationTime 2296),
    SimValueChange
      ( VectorChange
          (BinaryLower "0" (IdentifierCode "#"))
      ),
    SimValueChange
      ( ScalarChange
          (ScalarValueChange V1 (IdentifierCode "$"))
      )
  ]
