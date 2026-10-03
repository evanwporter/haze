{-# LANGUAGE OverloadedStrings #-}

module PipelineTest (pipelineTests) where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Parser
import Render
import Resample (resampleWaveform)
import Test.Tasty
import Test.Tasty.Golden
import qualified Text.Pretty.Simple as PS
import Transitions
import Types
import Waveform

pipelineTests :: TestTree
pipelineTests =
  testGroup
    "Pipeline Tests"
    [ goldenPipelineTest "test/vcd/logic.vcd" "test/golden/logic/",
      goldenPipelineTest "test/vcd/wikipedia.vcd" "test/golden/wikipedia/"
    ]

-- End-to-end pipeline test: VCD → Parse → Waveform → Resample → Ticks → Render
goldenPipelineTest :: FilePath -> FilePath -> TestTree
goldenPipelineTest vcdFile goldenPrefix =
  testGroup
    ("Pipeline: " ++ vcdFile)
    [ goldenVsString
        "parsed VCD"
        (goldenPrefix ++ "1-parsed.golden")
        $ parsedVCD vcdFile,
      goldenVsString
        "built waveform"
        (goldenPrefix ++ "2-waveform.golden")
        $ builtWaveform vcdFile,
      goldenVsString
        "resampled waveform"
        (goldenPrefix ++ "3-resampled.golden")
        $ resampledWaveform vcdFile,
      goldenVsString
        "generated ticks"
        (goldenPrefix ++ "4-ticks.golden")
        $ generatedTicks vcdFile,
      goldenVsString
        "colored chars (pre-render)"
        (goldenPrefix ++ "5-colored-chars.golden")
        $ coloredChars vcdFile,
      goldenVsString
        "rendered output (plain)"
        (goldenPrefix ++ "6-rendered.golden")
        $ renderedOutput vcdFile
    ]

parsedVCD :: FilePath -> IO BL.ByteString
parsedVCD vcdFile = do
  text <- TIO.readFile vcdFile
  pure . BL.pack $
    case parseOnly parseVCD text of
      Left err -> "Parse error: " ++ err
      Right vcd -> TL.unpack $ PS.pShowNoColor vcd

builtWaveform :: FilePath -> IO BL.ByteString
builtWaveform vcdFile = do
  text <- TIO.readFile vcdFile
  pure . BL.pack $
    case parseOnly parseVCD text of
      Left err -> "Parse error: " ++ err
      Right vcd ->
        let waveform = buildWaveValueMap (simulations vcd)
            normalized = sortOn fst $ HM.toList waveform
         in TL.unpack $ PS.pShowNoColor normalized

resampledWaveform :: FilePath -> IO BL.ByteString
resampledWaveform vcdFile = do
  text <- TIO.readFile vcdFile
  pure . BL.pack $
    case parseOnly parseVCD text of
      Left err -> "Parse error: " ++ err
      Right vcd ->
        let waveform = buildWaveValueMap (simulations vcd)
            resampled = resampleWaveform waveform
            normalized = sortOn fst $ HM.toList resampled
         in TL.unpack $ PS.pShowNoColor normalized

generatedTicks :: FilePath -> IO BL.ByteString
generatedTicks vcdFile = do
  text <- TIO.readFile vcdFile
  pure . BL.pack $
    case parseOnly parseVCD text of
      Left err -> "Parse error: " ++ err
      Right vcd ->
        let waveform = buildWaveValueMap (simulations vcd)
            resampled = resampleWaveform waveform
            withTicks = map (\(ident, vals) -> (ident, toRenderTicks vals 0)) (HM.toList resampled)
            normalized = sortOn fst withTicks
         in TL.unpack $ PS.pShowNoColor normalized

coloredChars :: FilePath -> IO BL.ByteString
coloredChars vcdFile = do
  text <- TIO.readFile vcdFile
  pure . BL.pack $
    case parseOnly parseVCD text of
      Left err -> "Parse error: " ++ err
      Right vcd ->
        let waveform = buildWaveValueMap (simulations vcd)
            resampled = resampleWaveform waveform
            signals = sortOn fst $ HM.toList resampled
            withColoredChars = map toColoredChars signals
            normalized = sortOn fst withColoredChars
         in TL.unpack $ PS.pShowNoColor normalized
  where
    toColoredChars (ident, values) =
      let ticks = toRenderTicks values 0
          colored = map (tickToColored defaultColors) ticks
       in (ident, colored)

renderedOutput :: FilePath -> IO BL.ByteString
renderedOutput vcdFile = do
  text <- TIO.readFile vcdFile
  pure . BL.pack $
    case parseOnly parseVCD text of
      Left err -> "Parse error: " ++ err
      Right vcd ->
        let waveform = buildWaveValueMap (simulations vcd)
            resampled = resampleWaveform waveform
            signals = sortOn fst $ HM.toList resampled
            rendered = map renderSignal signals
         in unlines rendered
  where
    renderSignal (IdentifierCode code, values) =
      let ticks = toRenderTicks values 0
       in T.unpack code ++ ": " ++ renderWaveTicks defaultColors ticks
