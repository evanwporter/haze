module Types where

import Haze

-- | Widget names used by the Haze application.
data Name = MainView
    deriving (Eq, Ord, Show)

-- | State shared by the Haze application and its UI layers.
data AppState = AppState
    { stateWaveConstruct :: WaveConstruct
    , stateCursor :: SimulationTime
    , symbolsShown :: [String]
    }

-- | A tick is set by the TimeScale in the VCD file header
columnsPerTick :: Int
columnsPerTick = 2

-- TODO: Move into state
valueBarWidth :: Int
valueBarWidth = 10
