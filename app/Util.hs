module Util where

import qualified Data.HashMap.Strict as HM
import Haze
import Types

-- | Width of a segment
segmentWidth :: Int -> Int
segmentWidth dur = columnsPerTick * (max 0 dur)
