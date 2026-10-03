{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Render
import System.Environment (getArgs)
import System.IO (hIsTerminalDevice, stdout)
import Transitions
import Types
import Waveform

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "test/vcd/logic.vcd" else head args

  -- Check if output is to a terminal (for color support)
  isTerm <- hIsTerminalDevice stdout

  putStrLn $ "Reading VCD file: " ++ filename
  putStrLn ""

  text <- TIO.readFile filename
  case parseOnly parseVCD text of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right vcd -> do
      let wf = buildWaveform (simulations vcd)
      let resampled = resampleWaveform wf
      let signals = sortOn fst $ HM.toList resampled

      putStrLn $ "Found " ++ show (length signals) ++ " signals"
      putStrLn $ "Colors: " ++ if isTerm then "enabled" else "disabled"

      -- Debug: show sample counts
      let sampleCounts = map (\(_, vals) -> length vals) signals
      putStrLn $ "Sample counts: " ++ show sampleCounts
      putStrLn ""

      -- Display each signal
      mapM_ (displaySignal isTerm) signals

displaySignal :: Bool -> (IdentifierCode, [WaveValue]) -> IO ()
displaySignal useColors (ident, values) = do
  let IdentifierCode code = ident
  let ticks = sampleWaveform values 0
  let rendered = renderWaveTicksColored defaultColors ticks

  putStrLn $ T.unpack code ++ ": " ++ rendered
  putStrLn ""
