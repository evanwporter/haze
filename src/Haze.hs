module Haze where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import Parser (parseText)
import Render
import Resample (maximumTime, minimumTime, resample)
import Transitions (toRenderTicks)
import Types
import Types (TimeNumber, TimeUnit)
import Waveform (WaveValueMap, buildWaveValueMap)

data WaveConstruct = WaveConstruct
    { wWaveform :: WaveValueMap
    , wMin :: SimulationTime
    , wMax :: SimulationTime
    -- wcTimescale :: (TimeNumber, TimeUnit)
    }

parseVCDFile :: FilePath -> IO (Either String WaveConstruct)
parseVCDFile path = do
    -- do unwraps the IO monad but we still need to handle
    -- the Either monad
    definitions <- parseText <$> TIO.readFile path

    -- (buildWaveValueMap . simulations) is a function that accepts
    -- ValueChangeDumpDefinitions
    -- <$> unwraps the either monad so it can operate underneath it
    let waveform = (buildWaveValueMap . simulations) <$> definitions

    let minTime = minimumTime <$> waveform
    let maxTime = maximumTime <$> waveform

    return $ WaveConstruct <$> waveform <*> minTime <*> maxTime

renderIdentifier :: IdentifierCode -> WaveConstruct -> String
renderIdentifier ident wave =
    -- We don't use a do statement here because we aren't returning
    -- a monad
    case HM.lookup ident (wWaveform wave) of
        Nothing -> "Signal Not Found"
        Just waveValues ->
            let resampled = resample (wMin wave) (wMax wave) waveValues
                renderTicks = toRenderTicks resampled
             in renderWaveTicks defaultColors renderTicks
