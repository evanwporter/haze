module Types where

import Haze

-- | Widget names used by the Haze application.
data Name = MainView
    deriving (Eq, Ord, Show)

-- | State owned by the signal-list pane.
data SignalListPaneState = SignalListPaneState
    { signalListSelectedSignal :: Int
    -- ^ The selected row in the complete list of available signals.
    }

-- | State owned by the waveform pane.
data WaveformPaneState = WaveformPaneState
    { waveformCursor :: SimulationTime
    , waveformDisplayedIdentifiers :: [IdentifierCode]
    , waveformSelectedIndex :: Maybe Int
    -- ^ The selected row among the signals displayed in the waveform.
    }

-- | State shared by the Haze application and its UI layers.
data AppState = AppState
    { stateWaveConstruct :: WaveConstruct
    , stateSignalList :: SignalListPaneState
    , stateWaveform :: WaveformPaneState
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

data Pane
    = SignalListPane
    | WaveformPane
    deriving (Eq, Show)
