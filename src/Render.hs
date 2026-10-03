module Render where

import Data.Maybe (fromMaybe)
import qualified System.Console.ANSI as ANSI
import Transitions

-- | Color scheme for waveforms
data WaveColors = WaveColors
  { -- | Color for high/active signals
    highColor :: ANSI.Color,
    -- | Color intensity for high/active signals
    highIntensity :: ANSI.ColorIntensity,
    -- | Color for low/inactive signals
    lowColor :: ANSI.Color,
    -- | Color intensity for low/inactive signals
    lowIntensity :: ANSI.ColorIntensity,
    -- | Color of the character overlay
    charColor :: ANSI.Color
  }

-- | Default color scheme (green/gray)
defaultColors :: WaveColors
defaultColors =
  WaveColors
    { highColor = ANSI.Green,
      highIntensity = ANSI.Vivid,
      lowColor = ANSI.Black,
      lowIntensity = ANSI.Vivid,
      charColor = ANSI.Black
    }

type ColorSpec = (ANSI.ColorIntensity, ANSI.Color)

data ColoredChar = ColoredChar
  { ccFg :: ColorSpec,
    ccBg :: ColorSpec,
    ccChar :: Char
  }
  deriving (Show)

-- | Utility function for building the color tuple from
-- a color scheme
highColors :: WaveColors -> ColorSpec
highColors colors = (highIntensity colors, highColor colors)

-- | Utility function for building the color tuple from
-- a color scheme
lowColors :: WaveColors -> ColorSpec
lowColors colors = (lowIntensity colors, lowColor colors)

-- | Utility function for building the color tuple from
-- a color scheme
charColors :: WaveColors -> ColorSpec
charColors colors = (ANSI.Dull, charColor colors)

-- | Convert tick to colored character with powerline blending
tickToColored :: WaveColors -> WaveTick -> ColoredChar
tickToColored colors (LogicTick state) = case state of
  LogicHigh ->
    ColoredChar
      (highColors colors)
      (highColors colors)
      ' '
  LogicLow ->
    ColoredChar
      (highColors colors)
      (lowColors colors)
      '▁'
  LogicRising ->
    ColoredChar
      (highColors colors)
      (lowColors colors)
      '\xE0BA' -- 
  LogicFalling ->
    ColoredChar
      (highColors colors)
      (lowColors colors)
      '\xE0B8' -- 
tickToColored colors (VectorTick state) = case state of
  VectorStable c ->
    ColoredChar
      -- The foreground doesn't matter here since we are rendering a space
      (charColors colors)
      (highColors colors)
      (fromMaybe ' ' c)
  VectorRight ->
    ColoredChar
      (highColors colors)
      (lowColors colors)
      '\xE0B0' -- 
  VectorLeft ->
    ColoredChar
      (highColors colors)
      (lowColors colors)
      '\xE0B2' -- 

-- | Apply ANSI codes to a colored character
renderColored :: ColoredChar -> String
renderColored (ColoredChar fg bg char) =
  ANSI.setSGRCode
    [ -- `uncurry` here unpacks the fg/bg tuple and passes it
      -- along to ANSI.SetColor as individual inputs
      uncurry (ANSI.SetColor ANSI.Foreground) fg,
      uncurry (ANSI.SetColor ANSI.Background) bg
    ]
    ++ [char]

renderWaveTicks :: WaveColors -> [WaveTick] -> String
renderWaveTicks colors ticks =
  concatMap (renderColored . tickToColored colors) ticks
    ++ ANSI.setSGRCode [ANSI.Reset]
