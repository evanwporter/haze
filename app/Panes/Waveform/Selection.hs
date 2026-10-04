module Panes.Waveform.Selection where

import Types

changeSelection :: Int -> AppState -> AppState
changeSelection diff state =
    let waveformState = stateWaveform state
        selectionLength = length (waveformDisplayedIdentifiers waveformState) - 1
        newIndex = (min selectionLength) <$> (max 0) <$> ((+ diff) <$> waveformSelectedIndex waveformState)
     in state
            { stateWaveform = waveformState{waveformSelectedIndex = newIndex}
            }

removeSignal :: AppState -> AppState
removeSignal state =
    let waveformState = stateWaveform state
        selectedIndex = waveformSelectedIndex waveformState
        displayedIdentifiers = waveformDisplayedIdentifiers waveformState

        -- get the selected identifier code
        selectedIdentifierCode = (displayedIdentifiers !!) <$> selectedIndex
     in -- remove (filter out) the selected IdentifierCode from the list of displayed
        -- identifer codes
        case selectedIdentifierCode of
            Nothing -> state
            Just ident ->
                state
                    { stateWaveform =
                        waveformState
                            { waveformDisplayedIdentifiers = filter (/= ident) displayedIdentifiers
                            }
                    }
