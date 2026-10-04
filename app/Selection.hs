module Selection where

import Haze
import Types

changeSelection :: Int -> AppState -> AppState
changeSelection diff state =
    let new_index = (+ diff) <$> stateSelectedIndex state
     in state
            { stateSelectedIndex = new_index
            }

removeSignal :: AppState -> AppState
removeSignal state =
    let selectedIndex = stateSelectedIndex state
        waveConstruct = stateWaveConstruct state
        identifierCodes = wcIdentifierCodes waveConstruct
        displayedIdentifiers = stateDisplayedIdentifiers state

        -- get the selected identifier code
        selectedIdentifierCode = (displayedIdentifiers !!) <$> selectedIndex
     in -- remove (filter out) the selected IdentifierCode from the list of displayed
        -- identifer codes
        case selectedIdentifierCode of
            Nothing -> state
            Just ident ->
                state
                    { stateDisplayedIdentifiers = filter (/= ident) (displayedIdentifiers)
                    }
