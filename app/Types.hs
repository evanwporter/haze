module Types where

import Haze

-- | Widget names used by the Haze application.
data Name = MainView
    deriving (Eq, Ord, Show)

-- | State shared by the Haze application and its UI layers.
data AppState = AppState
    { stateWaveConstruct :: WaveConstruct
    , stateCursor :: SimulationTime
    }
