{-# LANGUAGE OverloadedStrings #-}

module Panes.Waveform.TimeBar where

import Brick
import qualified Data.Text as T
import Haze
import Types

{- | A ruler aligned with the waveform's time axis.  Each simulation tick
occupies 'columnsPerTick' terminal columns; every fifth tick is labeled.
-}
timeBar :: AppState -> Widget n
timeBar state =
    txt $ T.concat $ map renderTick majorTicks
  where
    waveConstruct = stateWaveConstruct state
    SimulationTime maxTime = wMax waveConstruct

    ticksPerLabel = 5
    columnsPerLabel = ticksPerLabel * columnsPerTick
    majorTicks = [0, ticksPerLabel .. maxTime]

    renderTick :: Int -> T.Text
    renderTick tick =
        let label = T.pack (show tick)
            ruleWidth = max 0 (columnsPerLabel - T.length label)
         in label <> T.replicate ruleWidth "─"
