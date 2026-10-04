{-# LANGUAGE OverloadedStrings #-}

module Cursor where

import Brick
import Haze
import Types
import Util

cursorLayer :: AppState -> Widget Name
cursorLayer state =
    translateBy (Location (cursorX, 0)) $
        vBox $
            replicate (tableHeight state) (txt "|")
  where
    SimulationTime t = stateCursor state
    cursorX = t * columnsPerTick

moveCursor :: Int -> AppState -> AppState
moveCursor amount state =
    state
        { stateCursor = SimulationTime (max 0 (t + amount))
        }
  where
    SimulationTime t = stateCursor state
