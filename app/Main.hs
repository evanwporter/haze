{-# LANGUAGE OverloadedStrings #-}

module Main where

import Attributes
import Brick
import Brick.Widgets.Border
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import Haze
import System.Environment (getArgs)
import Types
import Waveform

data Name = MainView
    deriving (Eq, Ord, Show)

data AppState = AppState
    { entries :: [Widget Name]
    }

waveToWidget :: WaveSegmentData -> Widget n
waveToWidget (WaveSegmentData (LogicValue value) dur) =
    case value of
        V0 ->
            (withAttr lowAttr $ txt "\xE0B8") -- 
                <+> (withAttr lowAttr $ txt $ T.replicate (dur - 1) "▁▁")
                <+> (withAttr lowAttr $ txt "▁\xE0BA") -- 
        V1 ->
            (withAttr highAttr $ txt $ T.replicate (dur - 1) "  ")
                <+> (withAttr highAttr $ txt " ")
        _ -> withAttr highAttr $ txt $ T.replicate dur " "
waveToWidget (WaveSegmentData (BinaryValue text) dur) =
    let textLength = T.length text
        paddingLength =
            max
                0
                -- (dur - 1) accounts for the  and  on both sides
                --    (together they make up 1 tick over two columns)
                -- the * 2 accounts for the fact that a single tick
                -- takes up 2 columns
                ( ((dur - 1) * 2)
                    - textLength
                )
        content = text <> T.replicate paddingLength " "
     in (withAttr lowAttr $ txt "\xE0B2") -- 
            <+> (withAttr vectorCharAttr $ txt content)
            <+> (withAttr lowAttr $ txt "\xE0B0") -- 
waveToWidget (WaveSegmentData (RealValue number) dur) =
    withAttr vectorCharAttr $
        txt $
            T.take dur (T.pack (show number))

waveSegmentsToWidget :: [WaveSegmentData] -> Widget n
waveSegmentsToWidget [] = emptyWidget
waveSegmentsToWidget [segment] = waveToWidget segment
waveSegmentsToWidget (current : rest) =
    waveToWidget current
        <+> waveSegmentsToWidget rest

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
        , appHandleEvent = \_ -> return ()
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
