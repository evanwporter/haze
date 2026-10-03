{-# LANGUAGE OverloadedStrings #-}

module Main where

import Attributes
import Brick
import Brick.Widgets.Border
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Haze
import System.Environment (getArgs)
import Types
import Waveform

data Name = MainView
    deriving (Eq, Ord, Show)

data AppState = AppState
    { entries :: [Widget Name]
    }

columnsPerTick :: Int
columnsPerTick = 2

-- | Width of a segment
segmentWidth :: Int -> Int
segmentWidth dur = columnsPerTick * (max 0 dur)

logicBase :: Value -> Int -> Widget n
logicBase value width =
    case value of
        V0 -> withAttr lowAttr $ txt $ T.replicate width "▁"
        V1 -> withAttr highAttr $ txt $ T.replicate width " "
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

-- Set the rows that will be displayed
setEntries :: [Widget Name] -> AppState -> AppState
setEntries newEntries state =
    state{entries = newEntries}

initialState :: AppState
initialState =
    AppState
        { entries = []
        }

waveEntries :: WaveConstruct -> [Widget Name]
waveEntries waveConstruct =
    [ withAttr lowAttr (txt (code <> ": "))
        <+> waveSegmentsToWidget segments
    | (ident@(IdentifierCode code), _) <- sortOn fst $ HM.toList (wWaveform waveConstruct)
    , let maxTime = wMax waveConstruct
          segments = case HM.lookup ident (wWaveform waveConstruct) of
            Nothing -> []
            Just values -> constructWaveSegments maxTime values
    ]

table :: [Widget n] -> Widget n
table [] = emptyWidget
table [x] = x -- checks if there is one entry
table (x : xs) =
    x
        -- <=> places two widget vertically
        <=> hBorder
        <=> table xs

drawUI :: AppState -> [Widget Name]
drawUI state =
    [ joinBorders $
        border $
            ( table
                -- applies our widget creation function to every entry
                ( map
                    (padLeftRight 1)
                    -- returns the list of entries within state
                    (entries state)
                )
                -- this vertically places a fill widget which expands to take up
                -- all unused space
                <=> fill ' '
            )
    ]

app :: App AppState e Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \event -> case event of
            VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const waveAttrMap
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd] -> do
            waveConstructWrapped <- parseVCDFile cmd
            case waveConstructWrapped of
                Left err -> putStrLn err
                Right waveConstruct -> do
                    let state = setEntries (waveEntries waveConstruct) initialState
                    _ <- defaultMain app state
                    return ()
            putStrLn $ "Command was: " ++ cmd
        _ ->
            putStrLn "Usage: haze <command>"

    return ()
