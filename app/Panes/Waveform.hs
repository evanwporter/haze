{-# LANGUAGE OverloadedStrings #-}

module Panes.Waveform where

import Attributes
import Brick
import Brick.Widgets.Border
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Haze
import Types
import Util

logicBase :: Value -> Int -> Widget n
logicBase value width =
    case value of
        V0 -> withAttr lowAttr $ txt $ T.replicate width "▁"
        V1 -> withAttr highAttr $ txt $ T.replicate width " "
        -- TODO: handle Vx, VX, etc
        _ -> withAttr highAttr $ txt $ T.replicate width " "

waveToWidget :: WaveSegmentData -> Maybe WaveSegmentData -> Widget n
waveToWidget (WaveSegmentData (LogicValue value) dur) next =
    case (value, next) of
        -- V0 -> V1
        (V0, Just (WaveSegmentData (LogicValue V1) _)) ->
            logicBase V0 (max 0 (width - 1)) -- -1 to account for 
                <+> withAttr lowAttr (txt "\xE0BA") -- 

        -- V1 -> V0
        (V1, Just (WaveSegmentData (LogicValue V0) _)) ->
            logicBase V1 (max 0 (width - 1)) -- -1 to account for 
                <+> withAttr lowAttr (txt "\xE0B8") -- 

        -- V1 -> V1 or V0 -> V0
        _ -> logicBase value width
  where
    width = segmentWidth dur
waveToWidget (WaveSegmentData (BinaryValue text) dur) _ =
    let width = segmentWidth dur
        contentWidth = max 0 (width - 2)
        content = T.take contentWidth text
        paddingLength = contentWidth - T.length content
     in (withAttr lowAttr $ txt "\xE0B2") -- 
            <+> (withAttr vectorCharAttr $ txt (content <> T.replicate paddingLength " "))
            <+> (withAttr lowAttr $ txt "\xE0B0") -- 
waveToWidget (WaveSegmentData (RealValue number) dur) _ =
    let width = segmentWidth dur
        content = T.take width (T.pack (show number))
     in withAttr vectorCharAttr $
            txt $
                -- <> is used for combining two T.Text objects
                content <> T.replicate (width - T.length content) " "

waveSegmentsToWidget :: [WaveSegmentData] -> Widget n
waveSegmentsToWidget [] = emptyWidget
waveSegmentsToWidget (segment : rest) =
    waveToWidget segment next <+> waveSegmentsToWidget rest
  where
    next = case rest of
        [] -> Nothing
        nextSegment : _ -> Just nextSegment

initialState :: WaveConstruct -> AppState
initialState wave =
    AppState
        { stateWaveConstruct = wave
        , stateSignalList = SignalListPaneState {signalListSelectedSignal = 0}
        , stateWaveform =
            WaveformPaneState
                { waveformCursor = wMin wave
                , -- TODO: Don't display all wavemaps to start
                  waveformDisplayedIdentifiers = wcIdentifierCodes wave
                , waveformSelectedIndex = Just 0
                }
        }

-- TODO: Figure out what this does
waveEntries :: AppState -> WaveConstruct -> [Widget Name]
waveEntries state waveConstruct =
    [ waveSegmentsToWidget (constructWaveSegments maxTime values)
    | ident <- waveformDisplayedIdentifiers (stateWaveform state)
    , Just values <- [HM.lookup ident (wWaveform waveConstruct)]
    , let maxTime = wMax waveConstruct
    ]

{- | Takes a list of widgets and stacks them vertically with
a horizontal line between each.
-}
table :: [Widget n] -> Widget n
table [] = emptyWidget
table [x] = x -- checks if there is one entry
table (x : xs) =
    x
        -- <=> places two widget vertically
        <=> hBorder
        <=> table xs

waveformLayer :: AppState -> Widget Name
waveformLayer state =
    table
        -- applies our widget creation function to every entry
        ( map
            (padLeftRight 1)
            -- returns the list of entries within state
            (waveEntries state (stateWaveConstruct state))
        )
