{-# LANGUAGE OverloadedStrings #-}

module Cursor where

import Brick
import Haze
import ReferenceBar (referenceBar)
import Types

cursorLayer :: AppState -> Widget Name
cursorLayer state =
    translateBy (Location (cursorX, 1)) $
        vBox $
            replicate (height) (txt "|")
  where
    SimulationTime t = stateCursor state

    -- waveformLayer starts after the value bar, separator, and its left pad.
    -- The cursor is rendered as an independent layer, so this is its absolute
    -- screen position.
    cursorX = t * columnsPerTick + referenceBarWidth + valueBarWidth + 2

    height = ((length $ identifiersDisplayed state) * 2) - 1

moveCursor :: Int -> AppState -> AppState
moveCursor amount state =
    state
        { stateCursor = SimulationTime (max 0 (t + amount))
        }
  where
    SimulationTime t = stateCursor state
