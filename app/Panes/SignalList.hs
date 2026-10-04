{-# LANGUAGE OverloadedStrings #-}

module Panes.SignalList where

import Attributes
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
            mapMaybe signalRow identifierCodes
    )
        <+> vBorder
  where
    selectedIndex = signalListSelectedSignal (stateSignalList state)
    identifierCodes = wcIdentifierCodes waveConstruct
    selectedIdentifierCode = identifierCodes !! selectedIndex
    waveConstruct = stateWaveConstruct state
    symbolMap = wcSymbolMap waveConstruct

    signalRow :: IdentifierCode -> Maybe (Widget n)
    signalRow ident = do
        reference <- HM.lookup ident symbolMap
        let row = txt (signalLabel reference)
        if ident == selectedIdentifierCode
            then Just (withAttr selectedAttr row)
            else Just row

-- TODO: addSignal and removeSignal could be combined because they only have a
-- one line difference

addSignal :: AppState -> AppState
addSignal state =
    let selectedIndex = signalListSelectedSignal (stateSignalList state)
        waveConstruct = stateWaveConstruct state
        identifierCodes = wcIdentifierCodes waveConstruct

        -- get the selected identifier code
        selectedIdentifierCode = identifierCodes !! selectedIndex

        -- append the selected IdentifierCode to the list of displayed
        -- identifer codes
        waveformState = stateWaveform state
        displayedWaves = waveformDisplayedIdentifiers waveformState ++ [selectedIdentifierCode]
     in state
            { stateWaveform = waveformState {waveformDisplayedIdentifiers = displayedWaves}
            }

changeSelectedSignal :: Int -> AppState -> AppState
changeSelectedSignal diff state =
    -- TODO: Cap how high it can go; currently it crashes if we go
    -- to the max + 1
    let signalListState = stateSignalList state
        newIndex = max 0 (signalListSelectedSignal signalListState + diff)
     in state
            { stateSignalList = signalListState {signalListSelectedSignal = newIndex}
            }
