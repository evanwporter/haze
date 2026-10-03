{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Render
import Resample (resampleWaveform)
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

      -- Calculate tick width (based on first signal's tick count)
      let tickWidth = case signals of
            [] -> 0
            ((_, vals) : _) -> length (sampleWaveform vals 0)

      -- Display each signal with tick marks
      mapM_ (displaySignalWithTicks isTerm tickWidth) (zip [1 ..] signals)

displaySignalWithTicks :: Bool -> Int -> (Int, (IdentifierCode, [WaveValue])) -> IO ()
displaySignalWithTicks useColors tickWidth (idx, (ident, values)) = do
  let IdentifierCode code = ident
  let ticks = sampleWaveform values 0
  let rendered = renderWaveTicks defaultColors ticks
  let label = T.unpack code ++ ": "

  -- Print the waveform
  putStrLn $ label ++ rendered

  -- Print tick marks (every 10 ticks)
  -- Use the actual tick count, not the width parameter
  let actualTickCount = length ticks
  let tickLine = concat [if i `mod` 10 == 0 then "┊" else "·" | i <- [0 .. actualTickCount - 1]]
  -- Add 2 extra spaces to account for ANSI rendering offset
  putStrLn $ replicate (length label - 2) ' ' ++ tickLine
  putStrLn ""
