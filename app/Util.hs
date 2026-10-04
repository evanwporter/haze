module Util where

import Attributes
import Brick
import Brick.Widgets.Border
import Types

-- | Width of a segment
segmentWidth :: Int -> Int
segmentWidth dur = columnsPerTick * (max 0 dur)

-- https://github.com/Tritlo/tuispec/blob/main/example/app/Main.hs#L223-L230
paneBox :: AppState -> Pane -> String -> Widget Name -> Widget Name
paneBox state pane title content =
    overrideAttr borderAttr paneAttr $
        borderWithLabel titleWidget $
            padAll 1 (overrideAttr borderAttr internalBorderAttr content)
  where
    isFocused = stateFocusedPane state == pane
    paneAttr
        | isFocused = focusedPaneAttr
        | otherwise = unfocusedPaneAttr
    titleWidget
        | isFocused = withAttr focusedPaneAttr (str title)
        | otherwise = str title
