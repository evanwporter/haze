module Transitions where

import Types
import Wave

-- TODO: Implement null state

data LogicState
  = LogicHigh
  | LogicFalling
  | LogicRising
  | LogicLow
  deriving (Eq, Show)

data VectorState
  = VectorLeft (Maybe Char)
  | VectorRight (Maybe Char)
  | VectorStable (Maybe Char)
  deriving (Eq, Show)

data WaveTick
  = LogicTick LogicState
  | VectorTick VectorState
  deriving (Eq, Show)

-- constructWaveTick :: WaveValue -> [WaveTick]
-- constructWaveTick (LogicValue V0) = Binary
-- constructWaveTick (LogicValue V1) = '‾'
-- constructWaveTick (LogicValue Vx) = 'x'
-- constructWaveTick (LogicValue Vz) = 'z'
-- constructWaveTick (LogicValue VX) = 'X'
-- constructWaveTick (LogicValue VZ) = 'Z'
-- constructWaveTick (BinaryValue _) = '='
-- constructWaveTick (RealValue _) = '~'

buildWaveTicks :: [WaveValue] -> Int -> [WaveTick]
buildWaveTicks [] _ = []
buildWaveTicks (_ : []) _ = [] -- TODO: Thing about what exactly it should be
buildWaveTicks (curr : next : rest) index =
  case (curr, next) of
    (LogicValue V0, LogicValue V1) -> LogicTick LogicRising : LogicTick LogicHigh : buildWaveTicks rest 0
    (LogicValue V1, LogicValue V0) -> LogicTick LogicFalling : LogicTick LogicFalling : buildWaveTicks rest 0
    (LogicValue V0, LogicValue V0) -> LogicTick LogicLow : LogicTick LogicLow : buildWaveTicks rest 0
    (LogicValue V1, LogicValue V1) -> LogicTick LogicHigh : LogicTick LogicHigh : buildWaveTicks rest 0
    (BinaryValue v1, BinaryValue v2)
      | v1 == v2 -> []
      | otherwise -> []
