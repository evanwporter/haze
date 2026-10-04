{-# LANGUAGE OverloadedStrings #-}

module Attributes where

import Brick (AttrMap, AttrName, attrMap, attrName)
import Brick.Util (on)
import qualified Graphics.Vty as V

{- | Attribute used for a logic-high base.

Corresponds to:
  foreground = vivid green
  background = vivid green
-}
highAttr :: AttrName
highAttr = attrName "wave.high"

{- | Attribute used for a logic-low base.

This is also the color combination currently used by
rising/falling logic edges and left/right vector edges.

Corresponds to:
foreground = vivid green
background = terminal default
-}
lowAttr :: AttrName
lowAttr = attrName "wave.low"

{- | Attribute used for characters displayed inside vector values.

Corresponds to:
  foreground = dull black
  background = vivid green
-}
vectorCharAttr :: AttrName
vectorCharAttr = attrName "wave.vectorChar"

-- | Attribute used for the currently selected signal.
selectedAttr :: AttrName
selectedAttr = attrName "wave.selected"

focusedPaneAttr :: AttrName
focusedPaneAttr = attrName "pane.focused"

unfocusedPaneAttr :: AttrName
unfocusedPaneAttr = attrName "pane.unfocused"

-- | The normal color for borders that divide content inside a pane.
internalBorderAttr :: AttrName
internalBorderAttr = attrName "border.internal"

-- | General attribute map for waveform rendering.
waveAttrMap :: AttrMap
waveAttrMap =
    attrMap
        V.defAttr
        [ (highAttr, V.brightGreen `on` V.brightGreen)
        , (lowAttr, V.withForeColor V.defAttr V.brightGreen)
        , (vectorCharAttr, V.black `on` V.brightGreen)
        , (selectedAttr, V.black `on` V.brightYellow)
        , (focusedPaneAttr, V.withForeColor V.defAttr V.brightGreen)
        , (unfocusedPaneAttr, V.withForeColor V.defAttr V.brightBlack)
        , (internalBorderAttr, V.defAttr)
        ]
