{-# LANGUAGE OverloadedStrings #-}

module Main where

import Attributes
import Brick
import Brick.Widgets.Border
import Cursor
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Haze
import ReferenceBar
import Selection
import Panes.SignalList
import Panes.Waveform
import System.Environment (getArgs)
import TimeBar
import Types
import Util
import Values

drawUI :: AppState -> [Widget Name]
drawUI state =
    [ -- The cursor is a layer
      cursorLayer state
    , -- Next layer is everything else
      (signalList state)
        <+> ( ( txt (T.replicate referenceBarWidth " ")
                    <+> txt "│"
                    <+> hLimit valueBarWidth (txt cursorTimeText)
                    -- <+> txt "│"
                    <+> padLeft (Pad 1) (timeBar state)
              )
                <=> ( hLimit referenceBarWidth (table (referenceBar state))
                        <+> vBorder
                        <+> hLimit valueBarWidth (table (valueBar state))
                        <+> vBorder
                        <+> waveformLayer state
                    )
            )
    ]
  where
    SimulationTime cursorTime = waveformCursor (stateWaveform state)
    cursorTimeText =
        T.justifyLeft valueBarWidth ' ' (T.pack (show cursorTime))

app :: App AppState e Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \event -> case event of
            -- `q` to quit the TUI
            VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
            -- Left and Right to move the cursor around
            VtyEvent (V.EvKey (V.KChar 'h') []) ->
                modify (moveCursor (-1))
            VtyEvent (V.EvKey (V.KChar 'l') []) ->
                modify (moveCursor 1)
            -- move down 1 selection
            VtyEvent (V.EvKey (V.KChar 'j') []) ->
                modify (changeSelection 1)
            -- move up 1 selection in waveform display
            VtyEvent (V.EvKey (V.KChar 'k') []) ->
                modify (changeSelection (-1))
            VtyEvent (V.EvKey V.KDown []) ->
                modify (changeSelectedSignal (1))
            VtyEvent (V.EvKey V.KUp []) ->
                modify (changeSelectedSignal (-1))
            VtyEvent (V.EvKey (V.KChar 'd') []) ->
                modify removeSignal
            VtyEvent (V.EvKey V.KEnter []) ->
                modify addSignal
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
                    _ <- defaultMain app (initialState waveConstruct)
                    return ()
            putStrLn $ "Command was: " ++ cmd
        _ ->
            putStrLn "Usage: haze <command>"

    return ()
