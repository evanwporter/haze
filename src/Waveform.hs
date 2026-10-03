module Waveform where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Types

data WaveValue
  = LogicValue Value
  | BinaryValue T.Text
  | RealValue Double
  deriving (Eq, Show)

type Range = (Int, Int)

type Waveform = HM.HashMap IdentifierCode [(SimulationTime, WaveValue)]

addValueToWave :: IdentifierCode -> SimulationTime -> WaveValue -> Waveform -> Waveform
addValueToWave ident time val wave = HM.insertWith (++) ident [(time, val)] wave

addValuesToWave :: SimulationTime -> [ValueChange] -> Waveform -> Waveform
addValuesToWave _ [] wave = wave
addValuesToWave time (val : vals) wave =
  case val of
    -- We need to match it here to the constructor
    -- ie: we can't just match it to ScalarChange
    ScalarChange (ScalarValueChange value ident) ->
      addValuesToWave
        time
        vals
        (addValueToWave ident time (LogicValue value) wave)
    VectorChange (BinaryLower value ident) ->
      addValuesToWave
        time
        vals
        (addValueToWave ident time (BinaryValue value) wave)
    VectorChange (BinaryUpper value ident) ->
      addValuesToWave
        time
        vals
        (addValueToWave ident time (BinaryValue value) wave)
    VectorChange (RealLower value ident) ->
      addValuesToWave
        time
        vals
        (addValueToWave ident time (RealValue value) wave)
    VectorChange (RealUpper value ident) ->
      addValuesToWave
        time
        vals
        (addValueToWave ident time (RealValue value) wave)

parseSim :: SimulationTime -> SimulationCommand -> Waveform -> (SimulationTime, Waveform)
parseSim time sim wave = case sim of
  SimValueChange (val) -> case val of
    -- We need to match it here to the constructor
    -- ie: we can't just match it to ScalarChange
    ScalarChange (ScalarValueChange value ident) ->
      (time, addValueToWave ident time (LogicValue value) wave)
    VectorChange (BinaryLower value ident) ->
      (time, addValueToWave ident time (BinaryValue value) wave)
    VectorChange (BinaryUpper value ident) ->
      (time, addValueToWave ident time (BinaryValue value) wave)
    VectorChange (RealLower value ident) ->
      (time, addValueToWave ident time (RealValue value) wave)
    VectorChange (RealUpper value ident) ->
      (time, addValueToWave ident time (RealValue value) wave)
  SimTime (newTime) -> (newTime, wave)
  DumpVars (vals) -> (time, addValuesToWave time vals wave)
  _ -> (time, wave) -- for now we are skipping everything else

parseSims :: SimulationTime -> [SimulationCommand] -> Waveform -> Waveform
parseSims _ [] wave = wave
parseSims time (sim : sims) wave =
  let (newTime, newWave) = parseSim time sim wave
   in parseSims newTime sims newWave

buildWaveform :: [SimulationCommand] -> Waveform
buildWaveform sims =
  let initalWave = HM.empty
   in parseSims (SimulationTime 0) sims initalWave
