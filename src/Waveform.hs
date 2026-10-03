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

-- TODO: idk how correct this is. I believe its not required to dump the inital state
collectInitialValues :: [SimulationCommand] -> Waveform -> Either String Waveform
collectInitialValues [] _ = Left "No $dumpvars in VCD file"
collectInitialValues (sim : sims) wave = case sim of
  DumpVars (vals) -> Right (addValuesToWave (SimulationTime 0) vals wave)
  _ -> collectInitialValues sims wave

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

-- AI Generated; figure out what it does
-- resample :: Int -> Int -> Int -> [(Int, WaveValue)] -> [WaveValue]
-- resample start stop 1 changes =
--     go [start, start + 1 .. stop] changes
--   where
--     go [] _ = []
--     go _ [] = []
--     go (t : ts) ((tc, v) : rest) =
--         case rest of
--             (tn, _) : _
--                 | tn <= t -> go (t : ts) rest
--             _ -> v : go ts ((tc, v) : rest)

-- TODO: This is terribly inefficient
resample :: Int -> Int -> [(SimulationTime, WaveValue)] -> [WaveValue]
resample start stop changes =
  -- map means call sample on every one of these elements
  map sample [start .. stop]
  where
    orderedChanges = reverse changes

    -- where let's you define helper functions or vars used by the function above it
    sample t =
      -- takeWhile time is less than or equal to t
      -- take the last element of the list returned by takeWhile
      -- take the second element of the tuple returned by last
      snd $ last $ takeWhile (\((SimulationTime time), _) -> time <= t) orderedChanges

resampleWaveform :: Waveform -> HM.HashMap IdentifierCode [WaveValue]
resampleWaveform waveform =
  let SimulationTime start = minimumTime waveform
      SimulationTime stop = maximumTime waveform
   in -- Maps the resample function to every single value in the Waveform
      HM.map (resample start stop) waveform

minimumTime :: Waveform -> SimulationTime
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

maximumTime :: Waveform -> SimulationTime
maximumTime wf = fst $ maximumBy (comparing fst) (concat (HM.elems wf))
