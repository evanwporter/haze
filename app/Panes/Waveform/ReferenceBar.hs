{-# LANGUAGE OverloadedStrings #-}

module Panes.Waveform.ReferenceBar where

import Attributes
import Brick
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Haze
import Types

{- | Constructs the widget which displays the Reference text of the wave being
 - displayed in the corresponding row.
-}
referenceBar :: AppState -> [Widget n]
referenceBar state =
    -- We make the assumption that every identifier has a corresponding reference
    map buildWidget $ zip idents references
  where
    waveConstruct = stateWaveConstruct state
    identifierMap = wcSymbolMap waveConstruct
    idents = waveformDisplayedIdentifiers (stateWaveform state)
    references = mapMaybe (flip HM.lookup identifierMap) idents
    selectedIndex = waveformSelectedIndex (stateWaveform state)
    selectedIdent = (idents !!) <$> selectedIndex
    buildWidget (ident, reference)
        | Just ident == selectedIdent = withAttr selectedAttr (refToTxt reference)
        | otherwise = refToTxt reference
      where
        -- TODO: Make a function that displays these a little better
        refToTxt = txt . T.pack . show

-- buildWidget :: Widget n
-- buildWidget -> case
