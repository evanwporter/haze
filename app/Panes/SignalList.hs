{-# LANGUAGE OverloadedStrings #-}

module Panes.SignalList where

import Attributes
import Brick
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Haze
import Types

signalLabel :: Reference -> T.Text
signalLabel reference =
    T.justifyLeft signalListWidth ' ' (T.pack (show reference))

signalListPane :: AppState -> Widget n
signalListPane state =
    hLimit signalListWidth $
        vBox
            [ vBox (mapMaybe signalRow identifierCodes)
            , fill ' '
            ]
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
            { stateWaveform = waveformState{waveformDisplayedIdentifiers = displayedWaves}
            }

changeSelectedSignal :: Int -> AppState -> AppState
changeSelectedSignal diff state =
    let signalListState = stateSignalList state
        -- Clamp the new index
        newIndex = min selectionLength (max 0 (signalListSelectedSignal signalListState + diff))
        waveConstruct = stateWaveConstruct state
        identifierCodes = wcIdentifierCodes waveConstruct
        selectionLength = length identifierCodes - 1
     in state
            { stateSignalList = signalListState{signalListSelectedSignal = newIndex}
            }
