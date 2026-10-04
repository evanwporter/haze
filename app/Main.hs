{-# LANGUAGE OverloadedStrings #-}

module Main where

import Attributes
import Brick
import Brick.Widgets.Border (borderAttr, borderWithLabel)
import qualified Graphics.Vty as V
import Haze
import Panes.SignalList
import Panes.Waveform
import Panes.Waveform.Cursor
import Panes.Waveform.Selection
import System.Environment (getArgs)
import Types
import Util

drawUI :: AppState -> [Widget Name]
drawUI state =
    [ -- The cursor is a layer
      cursorLayer state
    , -- Next layer is everything else
      (paneBox state SignalListPane "Signals" (signalListPane state))
        <+> (paneBox state WaveformPane "Waveform" (waveformPane state))
    ]

app :: App AppState e Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \event -> case event of
            -- `q` to quit the TUI
            VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
            -- Tab moves keyboard focus between the two panes.
            VtyEvent (V.EvKey (V.KChar '\t') []) ->
                modify switchFocusedPane
            VtyEvent vtyEvent ->
                modify (handlePaneEvent vtyEvent)
            _ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const waveAttrMap
        }

switchFocusedPane :: AppState -> AppState
switchFocusedPane state =
    state
        { stateFocusedPane = case stateFocusedPane state of
            SignalListPane -> WaveformPane
            WaveformPane -> SignalListPane
        }

handlePaneEvent :: V.Event -> AppState -> AppState
handlePaneEvent event state =
    case stateFocusedPane state of
        SignalListPane -> case event of
            V.EvKey (V.KChar 'j') [] -> changeSelectedSignal 1 state
            V.EvKey (V.KChar 'k') [] -> changeSelectedSignal (-1) state
            V.EvKey V.KEnter [] -> addSignal state
            _ -> state
        WaveformPane -> case event of
            V.EvKey (V.KChar 'h') [] -> moveCursor (-1) state
            V.EvKey (V.KChar 'l') [] -> moveCursor 1 state
            V.EvKey (V.KChar 'j') [] -> changeSelection 1 state
            V.EvKey (V.KChar 'k') [] -> changeSelection (-1) state
            V.EvKey (V.KChar 'd') [] -> removeSignal state
            _ -> state

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd] -> do
            waveConstructWrapped <- parseVCDFile cmd
            case waveConstructWrapped of
                Left err -> putStrLn err
                Right waveConstruct -> do
                    _ <- defaultMain app (initialState waveConstruct)
                    return ()
            putStrLn $ "Command was: " ++ cmd
        _ ->
            putStrLn "Usage: haze <command>"

    return ()
