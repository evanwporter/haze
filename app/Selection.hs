module Selection where

import Types

changeSelection :: Int -> AppState -> AppState
changeSelection diff state =
    let new_index = (+ diff) <$> stateSelectedIndex state
     in state
            { stateSelectedIndex = new_index
            }
