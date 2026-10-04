{-# LANGUAGE OverloadedStrings #-}

module Panes.Waveform.Cursor where

import Brick
import Haze
import Types

cursorLayer :: AppState -> Widget Name
cursorLayer state =
    translateBy (Location (cursorX, 1)) $
        vBox $
            replicate (height) (txt "|") -- TODO: make this a solid background color or something nicer
  where
    waveformState = stateWaveform state
    SimulationTime t = waveformCursor waveformState

    -- waveformLayer starts after the reference bar, value bar, separators,
    -- and its left pad.  Time zero is the first waveform column.
    -- The cursor is rendered as an independent layer, so this is its absolute
    -- screen position.
    cursorX = t * columnsPerTick + waveformDisplayXOffset + 3

    height = (length (waveformDisplayedIdentifiers waveformState) * 2) - 1

moveCursor :: Int -> AppState -> AppState
moveCursor amount state =
    state {stateWaveform = waveformState {waveformCursor = SimulationTime (max 0 (t + amount))}}
  where
    waveformState = stateWaveform state
    SimulationTime t = waveformCursor waveformState
