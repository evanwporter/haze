{-# LANGUAGE OverloadedStrings #-}

module Panes.Waveform.Cursor where

import Brick
import Attributes
import Haze
import Types
import Panes.Waveform.Viewport

cursorLayer :: AppState -> Widget Name
cursorLayer state =
    translateBy (Location (cursorX, 3)) $
        vBox $
            replicate height (withAttr cursorAttr (txt " "))
  where
    waveformState = stateWaveform state
    SimulationTime t = waveformCursor waveformState
    SimulationTime viewStart = waveformViewportStart waveformState

    -- waveformLayer starts after the reference bar, value bar, separators,
    -- and its left pad.  Time zero is the first waveform column.
    -- The cursor is rendered as an independent layer, so this is its absolute
    -- screen position.
    cursorX = (t - viewStart) * columnsPerTick + waveformDisplayXOffset

    height = (length (waveformDisplayedIdentifiers waveformState) * 2) - 1

moveCursor :: Int -> AppState -> AppState
moveCursor amount state =
    keepCursorVisible $
        state{stateWaveform = waveformState{waveformCursor = newCursor}}
  where
    waveformState = stateWaveform state
    SimulationTime t = waveformCursor waveformState
    SimulationTime minTime = wMin (stateWaveConstruct state)
    SimulationTime maxTime = wMax (stateWaveConstruct state)
    newCursor = SimulationTime (min maxTime (max minTime (t + amount)))
