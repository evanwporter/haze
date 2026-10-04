module Util where

import Types

-- | Width of a segment
segmentWidth :: Int -> Int
segmentWidth dur = columnsPerTick * (max 0 dur)
