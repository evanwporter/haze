{-# LANGUAGE OverloadedStrings #-}

module Values where

import Brick
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Haze
import Types

-- | Values at the cursor, in the same identifier order as the waveform rows.
collectValues :: AppState -> [(IdentifierCode, WaveValue)]
collectValues state =
    let waveConstruct = stateWaveConstruct state
        waveMap = wWaveform waveConstruct
        cursorTime = stateCursor state
     in sortOn -- sort by indentifier code
            fst
            [ (ident, value)
            | (ident, entries) <- HM.toList waveMap
            , Just value <- [findValueAtCursor cursorTime entries]
            ]
  where
    -- Gets the most recent change at or before the cursor, not the next change after it.
    findValueAtCursor :: SimulationTime -> [(SimulationTime, WaveValue)] -> Maybe WaveValue
    findValueAtCursor targetTime = go Nothing
      where
        go :: Maybe WaveValue -> [(SimulationTime, WaveValue)] -> Maybe WaveValue
        go current [] = current
        go current ((time, value) : rest)
            | time <= targetTime = go (Just value) rest
            | otherwise = current

{- | Constructs the widget which displays the Values of where the
cursor has landed
-}
valueBar :: AppState -> [Widget n]
valueBar state =
    buildWidgets values
  where
    values = map snd (collectValues state)

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
