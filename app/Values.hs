{-# LANGUAGE OverloadedStrings #-}

module Values where

import Brick
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Haze
import Types

collectValues :: AppState -> HM.HashMap IdentifierCode WaveValue
collectValues state =
    -- state
    let waveConstruct = stateWaveConstruct state
        waveMap = wWaveform waveConstruct
        cursorTime = stateCursor state
     in -- mapMaybe drops entries where we get a Nothing value
        HM.mapMaybe (findFirstEntryGreaterThan cursorTime) waveMap
  where
    findFirstEntryGreaterThan :: SimulationTime -> [(SimulationTime, WaveValue)] -> Maybe WaveValue
    findFirstEntryGreaterThan _ [] = Nothing
    findFirstEntryGreaterThan targetTime ((time, value) : rest)
        | targetTime <= time = Just value
        | otherwise = findFirstEntryGreaterThan targetTime rest

valueBar :: AppState -> [Widget n]
valueBar state =
    buildWidgets values
  where
    values = HM.elems (collectValues state)

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
