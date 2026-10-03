module Transitions where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Resample
import Types
import Waveform

-- TODO: Implement null state (ie: x and X)

type RenderTickMap = HM.HashMap IdentifierCode [RenderTick]

data LogicState
    = LogicHigh
    | LogicFalling
    | LogicRising
    | LogicLow
    deriving (Eq, Show)

data VectorState
    = VectorLeft
    | VectorRight
    | VectorStable (Maybe Char)
    deriving (Eq, Show)

data RenderTick
    = LogicTick LogicState
    | VectorTick VectorState
    deriving (Eq, Show)

-- TODO: Eventually this will need to be able to a zoom parameters
-- Which is essentially how wide each value should be. For now its hardcoded at 2
-- Add time

-- | Takes the list of value changes and upsamples it into a list of tick by ticks range
toRenderTicks :: [WaveValue] -> [RenderTick]
toRenderTicks input = toRenderTicksHelper input 0

toRenderTicksHelper :: [WaveValue] -> Int -> [RenderTick]
toRenderTicksHelper [] _ = []
-- Check if its a single value
toRenderTicksHelper [val] index =
    case val of
        LogicValue V0 -> [LogicTick LogicLow, LogicTick LogicLow]
        LogicValue V1 -> [LogicTick LogicHigh, LogicTick LogicHigh]
        LogicValue _ -> []
        BinaryValue v ->
            let tick1 =
                    if index < T.length v
                        then VectorTick (VectorStable (Just (T.index v index)))
                        else VectorTick (VectorStable Nothing)
                tick2 =
                    if (index + 1) < T.length v
                        then VectorTick (VectorStable (Just (T.index v (index + 1))))
                        else VectorTick (VectorStable Nothing)
             in [tick1, tick2]
        _ -> []
toRenderTicksHelper (curr : next : rest) index =
    case (curr, next) of
        (LogicValue V0, LogicValue V1) -> LogicTick LogicRising : LogicTick LogicHigh : toRenderTicksHelper (next : rest) 0
        (LogicValue V1, LogicValue V0) -> LogicTick LogicFalling : LogicTick LogicLow : toRenderTicksHelper (next : rest) 0
        (LogicValue V0, LogicValue V0) -> LogicTick LogicLow : LogicTick LogicLow : toRenderTicksHelper (next : rest) 0
        (LogicValue V1, LogicValue V1) -> LogicTick LogicHigh : LogicTick LogicHigh : toRenderTicksHelper (next : rest) 0
        (BinaryValue v1, BinaryValue v2)
            -- the binary values equal each other in which case we want to return the vector stable
            -- with the index and index + 1th character
            | v1 == v2 && (T.length v1) >= (index + 2) ->
                VectorTick (VectorStable (Just (T.index v1 index)))
                    : VectorTick (VectorStable (Just (T.index v1 (index + 1))))
                    : toRenderTicksHelper (next : rest) (index + 2)
            | v1 == v2 && (T.length v1 == index + 1) ->
                VectorTick (VectorStable (Just (T.index v1 index)))
                    : VectorTick (VectorStable (Nothing))
                    : toRenderTicksHelper (next : rest) (index + 2)
            | v1 == v2 -> -- (T.length v1 <= index)
                VectorTick (VectorStable (Nothing))
                    : VectorTick (VectorStable (Nothing))
                    : toRenderTicksHelper (next : rest) (index + 1)
            | v1 /= v2 ->
                VectorTick (VectorRight)
                    : VectorTick (VectorLeft)
                    : toRenderTicksHelper (next : rest) (0)
            | otherwise -> []
        _ -> []

sampleWaveforms :: DenseWaveValueMap -> RenderTickMap
-- flip revesers the arguments so that I can pass the index first
-- and the waves map second
sampleWaveforms waves = HM.map (flip toRenderTicksHelper 0) waves
