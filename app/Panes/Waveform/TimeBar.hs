{-# LANGUAGE OverloadedStrings #-}

module Panes.Waveform.TimeBar where

import Brick
import qualified Data.Text as T
import Haze
import Panes.Waveform.Viewport
import Types

{- | A ruler aligned with the waveform's time axis.  Each simulation tick
occupies 'columnsPerTick' terminal columns; every fifth tick is labeled.
-}
timeBar :: AppState -> Widget n
timeBar state =
    txt $ T.replicate columnsBeforeFirstLabel "─" <> T.concat (map renderTick majorTicks)
  where
    waveformState = stateWaveform state
    SimulationTime viewStart = waveformViewportStart waveformState
    SimulationTime viewEnd = waveformViewEnd state

    ticksPerLabel = 5
    columnsPerLabel = ticksPerLabel * columnsPerTick
    firstLabelTick = ((viewStart + ticksPerLabel - 1) `div` ticksPerLabel) * ticksPerLabel
    columnsBeforeFirstLabel = (firstLabelTick - viewStart) * columnsPerTick
    majorTicks = [firstLabelTick, firstLabelTick + ticksPerLabel .. viewEnd]

    renderTick :: Int -> T.Text
    renderTick tick =
        let label = T.pack (show tick)
            ruleWidth = max 0 (columnsPerLabel - T.length label)
         in label <> T.replicate ruleWidth "─"
