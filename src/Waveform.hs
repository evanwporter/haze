module Waveform where

import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Types

data WaveValue
    = LogicValue Value
    | BinaryValue T.Text
    | RealValue Double
    deriving (Eq, Show)

type Range = (Int, Int)

type WaveValueMap = HM.HashMap IdentifierCode [(SimulationTime, WaveValue)]

addValueToWave :: IdentifierCode -> SimulationTime -> WaveValue -> WaveValueMap -> WaveValueMap
addValueToWave ident time val wave = HM.insertWith (++) ident [(time, val)] wave

addValuesToWave :: SimulationTime -> [ValueChange] -> WaveValueMap -> WaveValueMap
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

parseSim :: SimulationTime -> SimulationCommand -> WaveValueMap -> (SimulationTime, WaveValueMap)
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

parseSims :: SimulationTime -> [SimulationCommand] -> WaveValueMap -> WaveValueMap
parseSims _ [] wave = wave
parseSims time (sim : sims) wave =
    let (newTime, newWave) = parseSim time sim wave
     in parseSims newTime sims newWave

buildWaveValueMap :: [SimulationCommand] -> WaveValueMap
buildWaveValueMap sims =
    let initalWave = HM.empty
     in padWave (parseSims (SimulationTime 0) sims initalWave)

{- | Pad both sides so it reaches the minimum and maximum times
And it also reverses the list
-}
padWave :: WaveValueMap -> WaveValueMap
padWave waves = HM.map (padBothSides) waves
  where
    padBothSides = (pad minTime) . reverse . pad (maxTime)

    minTime = minimumTime waves
    maxTime = maximumTime waves

    pad :: SimulationTime -> [(SimulationTime, WaveValue)] -> [(SimulationTime, WaveValue)]
    pad _ [] = []
    pad minTime ((time, value) : rest)
        | time == minTime = (time, value) : rest
        | otherwise = (minTime, value) : (time, value) : rest

minimumTime :: WaveValueMap -> SimulationTime
minimumTime wf =
    -- intermediate map
    -- a map of identifier code to minimum SimulationTime
    -- let nm = HM.map (minimumBy (comparing fst)) wf
    --  in fst $ minimumBy (comparing fst) (HM.elems nm)
    --
    -- 1) obtain the elements of the waveform. This return a list of lists
    -- 2) flatten the lists of list into a list
    -- 3) obtain the minimum by only looking at the first element
    -- 4) obtain the first element from the resulting tuple
    fst $ minimumBy (comparing fst) (concat (HM.elems wf))

maximumTime :: WaveValueMap -> SimulationTime
maximumTime wf = fst $ maximumBy (comparing fst) (concat (HM.elems wf))
