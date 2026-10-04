-- TODO: AI Generated
module Panes.Waveform.Viewport where

import Haze
import Types

-- | The number of simulation ticks that fit in the waveform drawing area.
waveformVisibleTicks :: WaveformPaneState -> Int
waveformVisibleTicks waveformState =
    max 1 (waveformViewportWidth waveformState `div` columnsPerTick)

-- | The final simulation time currently visible in the waveform pane.
waveformViewEnd :: AppState -> SimulationTime
waveformViewEnd state =
    SimulationTime (min maxTime (viewStart + waveformVisibleTicks waveformState))
  where
    waveConstruct = stateWaveConstruct state
    waveformState = stateWaveform state
    SimulationTime maxTime = wMax waveConstruct
    SimulationTime viewStart = waveformViewportStart waveformState

-- | Update the drawable waveform width after Brick reports a terminal resize.
resizeWaveformViewport :: Int -> AppState -> AppState
resizeWaveformViewport terminalWidth state =
    state
        { stateWaveform =
            waveformState
                { waveformViewportWidth = max columnsPerTick availableWidth
                }
        }
  where
    waveformState = stateWaveform state

    -- Reserve the columns before the waveform, plus the waveform pane's
    -- right padding and border.
    availableWidth = terminalWidth - waveformDisplayXOffset - 2

{- | Keep the cursor within the visible time range.  The viewport moves only
when the cursor tries to leave it.
-}
keepCursorVisible :: AppState -> AppState
keepCursorVisible state =
    state{stateWaveform = waveformState{waveformViewportStart = newViewStart}}
  where
    waveformState = stateWaveform state
    SimulationTime cursor = waveformCursor waveformState
    SimulationTime viewStart = waveformViewportStart waveformState
    SimulationTime viewEnd = waveformViewEnd state
    SimulationTime minTime = wMin (stateWaveConstruct state)
    visibleTicks = waveformVisibleTicks waveformState

    newViewStart = clampToMinimum requestedViewStart

    requestedViewStart
        | cursor < viewStart = cursor
        | cursor > viewEnd = cursor - visibleTicks
        | otherwise = viewStart

    -- The left edge cannot precede the beginning of the simulation.
    clampToMinimum start = SimulationTime (max minTime start)
