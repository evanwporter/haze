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
  let filename = if null args then "test/vcd/wikipedia.vcd" else head args

  -- Check if output is to a terminal (for color support)
  isTerm <- hIsTerminalDevice stdout

  putStrLn $ "Reading VCD file: " ++ filename
  putStrLn ""

  text <- TIO.readFile filename
  case parseOnly parseVCD text of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right vcd -> do
      let wf = buildWaveform (simulations vcd)
      let signals = sortOn fst $ HM.toList wf

      putStrLn $ "Found " ++ show (length signals) ++ " signals"
      putStrLn $ "Colors: " ++ if isTerm then "enabled" else "disabled"
      putStrLn ""

      -- Get time range
      let times = concatMap (map fst . snd) signals
      let tMin = if null times then 0 else minimum [t | SimulationTime t <- times]
      let tMax = if null times then 100 else maximum [t | SimulationTime t <- times]

      putStrLn $ "Time range: " ++ show tMin ++ " to " ++ show tMax
      putStrLn ""

      -- Resample and display each signal
      mapM_ (displaySignal isTerm wf tMin tMax) signals

displaySignal :: Bool -> Waveform -> Int -> Int -> (IdentifierCode, [(SimulationTime, WaveValue)]) -> IO ()
displaySignal useColors wf tMin tMax (ident, changes) = do
  let IdentifierCode code = ident
  let resampled = resample tMin tMax changes
  let ticks = sampleWaveform resampled 0
  let rendered =
        renderWaveTicksColored defaultColors ticks

  putStrLn $ T.unpack code ++ ": " ++ rendered
  putStrLn "" -- Add blank line between signals
