module Util where

import qualified Data.HashMap.Strict as HM
import Haze
import Types

-- TODO: Derive this from the symbolsShown
tableHeight :: AppState -> Int
tableHeight state =
    case HM.size (wWaveform (stateWaveConstruct state)) of
        0 -> 0
        n -> 2 * n - 1

-- | Width of a segment
segmentWidth :: Int -> Int
segmentWidth dur = columnsPerTick * (max 0 dur)
