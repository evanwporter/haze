module Resample (
    DenseWaveValueMap,
    resample,
    resampleWaveform,
    minimumTime,
    maximumTime,
)
where

import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Types
import Waveform

type DenseWaveValueMap = HM.HashMap IdentifierCode [WaveValue]

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
-- Converts a sparse list of (SimulationTime, WaveValue) pairs into a dense list of [WaveValue]
resample :: SimulationTime -> SimulationTime -> [(SimulationTime, WaveValue)] -> [WaveValue]
-- This unwraps the SimulationTime
resample (SimulationTime start) (SimulationTime stop) changes =
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

resampleWaveform :: WaveValueMap -> DenseWaveValueMap
resampleWaveform waveform =
    -- Maps the resample function to every single value in the WaveValueMap
    HM.map (resample (minimumTime waveform) (maximumTime waveform)) waveform
