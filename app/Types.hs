module Types where

import Haze

-- | Widget names used by the Haze application.
data Name = MainView
    deriving (Eq, Ord, Show)

-- TODO: Think about splitting up the states for waveform and selection

-- | State shared by the Haze application and its UI layers.
data AppState = AppState
    { stateWaveConstruct :: WaveConstruct
    , stateCursor :: SimulationTime
    , identifiersDisplayed :: [IdentifierCode]
    , stateSelectedIndex :: Maybe Int
    {- ^ The selected identifer / row index in the list of identifiersDisplayed.
    This is the selected wave for deleting or move waves.
    TODO: Make it a list eventually
    -}
    , stateSelectedSignal :: Int
    {- ^ The currently selected signal in the SignalList. Corresponds to the
    entry in wcIdentifierCodes
    -}
    }

-- | A tick is set by the TimeScale in the VCD file header
columnsPerTick :: Int
columnsPerTick = 2

-- TODO: Move into state
valueBarWidth :: Int
valueBarWidth = 10

referenceBarWidth :: Int
referenceBarWidth = 20

signalListWidth :: Int
signalListWidth = 15

waveformDisplayXOffset :: Int
waveformDisplayXOffset = signalListWidth + valueBarWidth + referenceBarWidth
