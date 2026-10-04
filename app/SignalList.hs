{-# LANGUAGE OverloadedStrings #-}

module SignalList where

import Brick
import Brick.Widgets.Border (vBorder)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Haze
import Types

signalLabel :: Reference -> T.Text
signalLabel reference =
    T.justifyLeft signalListWidth ' ' (T.pack (show reference))

signalList :: AppState -> Widget n
signalList state =
    ( hLimit signalListWidth $
        vBox $
            map (txt . signalLabel) references
    )
        <+> vBorder
  where
    waveConstruct = stateWaveConstruct state
    symbolMap = wcSymbolMap waveConstruct
    references = mapMaybe (flip HM.lookup symbolMap) (wcIdentifierCodes waveConstruct)

-- addSignal :: AppState -> AppState
-- addSignal state =
--     let selectedIndex = stateSelectedSignal state
--         waveConstruct = stateWaveConstruct state
--         symbolMap = wcSymbolMap waveConstruct
--      in -- we need to use selectedIndex to grab the corresponding index
--         state
