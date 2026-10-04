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

-- | General attribute map for waveform rendering.
waveAttrMap :: AttrMap
waveAttrMap =
    attrMap
        V.defAttr
        [ (highAttr, V.brightGreen `on` V.brightGreen)
        , (lowAttr, V.withForeColor V.defAttr V.brightGreen)
        , (vectorCharAttr, V.black `on` V.brightGreen)
        ]
