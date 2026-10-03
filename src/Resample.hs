module Resample where

import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Types
import Waveform

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
