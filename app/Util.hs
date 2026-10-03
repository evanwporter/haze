module Util where

import Attributes
import Brick
import Brick.Widgets.Border
import Cursor
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Haze
import System.Environment (getArgs)
import Types

tableHeight :: AppState -> Int
tableHeight state =
    case HM.size (wWaveform (stateWaveConstruct state)) of
        0 -> 0
        n -> 2 * n - 1
