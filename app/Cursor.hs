{-# LANGUAGE OverloadedStrings #-}

module Cursor where

import Brick
import Haze
import Types
import Util

cursorLayer :: AppState -> Widget Name
cursorLayer state =
    translateBy (Location (cursorX, 1)) $
        vBox $
            replicate (tableHeight state) (txt "|")
  where
    SimulationTime t = stateCursor state
    -- waveformLayer starts after the value bar, separator, and its left pad.
    -- The cursor is rendered as an independent layer, so this is its absolute
    -- screen position.
    cursorX = t * columnsPerTick + valueBarWidth + 2

moveCursor :: Int -> AppState -> AppState
moveCursor amount state =
    state
        { stateCursor = SimulationTime (max 0 (t + amount))
        }
  where
    SimulationTime t = stateCursor state
