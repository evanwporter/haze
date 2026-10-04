{-# LANGUAGE OverloadedStrings #-}

module ReferenceBar where

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
    -- TODO: Make a function that displays these a little better
    map (txt . T.pack . show) references
  where
    waveConstruct = stateWaveConstruct state
    identifierMap = wcSymbolMap waveConstruct
    idents = identifiersDisplayed state
    references = mapMaybe (flip HM.lookup identifierMap) idents
