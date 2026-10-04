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

-- TODO: addSignal and removeSignal could be combined because they only have a
-- one line difference

addSignal :: AppState -> AppState
addSignal state =
    let selectedIndex = stateSelectedSignal state
        waveConstruct = stateWaveConstruct state
        identifierCodes = wcIdentifierCodes waveConstruct

        -- get the selected identifier code
        selectedIdentifierCode = identifierCodes !! selectedIndex

        -- append the selected IdentifierCode to the list of displayed
        -- identifer codes
        displayedWaves = identifiersDisplayed state ++ [selectedIdentifierCode]
     in state
            { identifiersDisplayed = displayedWaves
            }

removeSignal :: AppState -> AppState
removeSignal state =
    let selectedIndex = stateSelectedSignal state
        waveConstruct = stateWaveConstruct state
        identifierCodes = wcIdentifierCodes waveConstruct

        -- get the selected identifier code
        selectedIdentifierCode = identifierCodes !! selectedIndex

        -- remove (filter out) the selected IdentifierCode from the list of displayed
        -- identifer codes
        displayedWaves = filter (/= selectedIdentifierCode) (identifiersDisplayed state)
     in state
            { identifiersDisplayed = displayedWaves
            }
