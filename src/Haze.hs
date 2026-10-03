module Haze where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser (parseText)
import Resample (maximumTime, minimumTime)
import Types
import Waveform (WaveValueMap, buildWaveform)

data WaveConstruct = WaveConstruct
  { wWaveform :: WaveValueMap,
    wMin :: SimulationTime,
    wMax :: SimulationTime
  }

parseVCDFile :: FilePath -> IO (Either String WaveConstruct)
parseVCDFile path = do
  -- do unwraps the IO monad but we still need to handle
  -- the Either monad
  definitions <- parseText <$> TIO.readFile path

  -- (buildWaveform . simulations) is a function that accepts
  -- ValueChangeDumpDefinitions
  -- <$> unwraps the either monad so it can operate underneath it
  let waveform = (buildWaveform . simulations) <$> definitions

  let minTime = minimumTime <$> waveform
  let maxTime = maximumTime <$> waveform

  return $ WaveConstruct <$> waveform <*> minTime <*> maxTime

-- buildWaveConstruct :: T.Text -> Either String WaveConstruct
-- buildWaveConstruct input =
