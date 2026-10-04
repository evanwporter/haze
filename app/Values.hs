{-# LANGUAGE OverloadedStrings #-}

module Values where

import Brick
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Haze
import Types

{- | Values for each displayed identifier/wave according to where the cursor
is. The returned list is in the same order as the stateDisplayedIdentifiers.
-}
collectValues :: AppState -> [WaveValue]
collectValues state =
    mapMaybe
        collectValue
        (stateDisplayedIdentifiers state)
  where
    waveConstruct = stateWaveConstruct state
    waveMap = wWaveform waveConstruct
    cursorTime = stateCursor state

    collectValue :: IdentifierCode -> Maybe WaveValue
    collectValue ident = do
        entry <- HM.lookup ident waveMap
        value <- findValueAtCursor cursorTime entry
        return value

    -- Gets the most recent change at or before the cursor, not the next change after it.
    findValueAtCursor :: SimulationTime -> [(SimulationTime, WaveValue)] -> Maybe WaveValue
    findValueAtCursor _ [] = Nothing
    findValueAtCursor targetTime ((time, value) : rest)
        | time > targetTime = Nothing
        | otherwise =
            case findValueAtCursor targetTime rest of
                Nothing -> Just value
                foundValue -> foundValue

{- | Constructs the widget which displays the Values of where the
cursor has landed
-}
valueBar :: AppState -> [Widget n]
valueBar state =
    buildWidgets values
  where
    values = collectValues state

    buildWidgets :: [WaveValue] -> [Widget n]
    buildWidgets [] = []
    buildWidgets (val : rest) =
        let v = case val of
                LogicValue logic -> case logic of
                    V0 -> txt "0"
                    V1 -> txt "1"
                    Vx -> txt "x"
                    VX -> txt "X"
                    Vz -> txt "z"
                    VZ -> txt "Z"
                BinaryValue bin -> txt bin
                RealValue number ->
                    txt (T.pack (show number))
         in v : buildWidgets rest
