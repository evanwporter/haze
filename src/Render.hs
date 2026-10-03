module Render where

import qualified System.Console.ANSI as ANSI
import Transitions

-- Color scheme for waveforms
data WaveColors = WaveColors
  { highColor :: ANSI.Color,
    lowColor :: ANSI.Color
  }

-- Default color scheme (green/gray)
defaultColors :: WaveColors
defaultColors =
  WaveColors
    { highColor = ANSI.Green,
      lowColor = ANSI.Black
    }

-- Colored character with fg/bg for powerline blending
data ColoredChar = ColoredChar
  { ccFg :: ANSI.Color,
    ccBg :: ANSI.Color,
    ccChar :: String
  }

-- Convert tick to colored character with powerline blending
tickToColored :: WaveColors -> WaveTick -> ColoredChar
tickToColored colors (LogicTick state) = case state of
  LogicHigh ->
    ColoredChar
      (highColor colors)
      (highColor colors)
      "▔"
  LogicLow ->
    ColoredChar
      (highColor colors)
      (lowColor colors)
      "▁"
  LogicRising ->
    ColoredChar
      (highColor colors)
      (lowColor colors)
      "\xE0BA"
  LogicFalling ->
    ColoredChar
      (highColor colors)
      (lowColor colors)
      ("\xE0B8")
tickToColored colors (VectorTick _) = ColoredChar (highColor colors) (lowColor colors) (" ")

-- Apply ANSI codes to a colored character
renderColored :: ColoredChar -> String
renderColored (ColoredChar fg bg char) =
  ANSI.setSGRCode
    [ ANSI.SetColor ANSI.Foreground ANSI.Vivid fg,
      ANSI.SetColor ANSI.Background ANSI.Vivid bg
    ]
    ++ char

renderWaveTicksColored :: WaveColors -> [WaveTick] -> String
renderWaveTicksColored colors ticks =
  concatMap (renderColored . tickToColored colors) ticks
    ++ ANSI.setSGRCode [ANSI.Reset]
