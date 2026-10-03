module Haze where

import Data.HashMap.Internal.Debug (SubHash)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import Parser (parseText)
import Render
import Resample (maximumTime, minimumTime, resample)
import Transitions (toRenderTicks)
import Types
import Waveform (WaveValue, WaveValueMap, buildWaveValueMap)

data WaveConstruct = WaveConstruct
    { wWaveform :: WaveValueMap
    , wMin :: SimulationTime
    , wMax :: SimulationTime
    , wcTimescale :: (TimeNumber, TimeUnit)
    }

{- | A Wave Segment is the section of the wave from
one value change to the next.
-}
data WaveSegmentData = WaveSegmentData
    { waveSegmentValue :: WaveValue
    -- ^ The value change of the segment
    , duration :: Int
    {- ^ The duration is how many ticks it lasts. A tick
    being the globally set TimeScale. Its calculated by
    subtracting the current SimulationTime from the previous
    SimulationTime.
    -}
    }

constructWaveSegments :: SimulationTime -> [(SimulationTime, WaveValue)] -> [WaveSegmentData]
constructWaveSegments _ [] = []
constructWaveSegments maxTime [curr] =
    let SimulationTime currTime = fst curr
        SimulationTime nextTime = maxTime
        value = snd curr
        waveSegmentData = WaveSegmentData value (nextTime - currTime)
     in [waveSegmentData]
constructWaveSegments maxTime (curr : next : rest) =
    let SimulationTime currTime = fst curr
        SimulationTime nextTime = fst next
        value = snd curr
        waveSegmentData = WaveSegmentData value (nextTime - currTime)
     in waveSegmentData : constructWaveSegments maxTime (next : rest)

getTimescale :: [DeclarationCommand] -> Either String (TimeNumber, TimeUnit)
getTimescale [] = Left "There is no TimeScale in the waveform header."
getTimescale (decl : rest) = case decl of
    TimeScale timeNumber timeUnit ->
        Right (timeNumber, timeUnit)
    _ -> getTimescale rest

-- Ideally we don't use the IO monad here but I'm keeping it because its teaching me
-- a lot about dealing with nested monads
parseVCDFile :: FilePath -> IO (Either String WaveConstruct)
parseVCDFile path = do
    -- do unwraps the IO monad but we still need to handle
    -- the Either monad
    definitions <- parseText <$> TIO.readFile path

    -- (buildWaveValueMap . simulations) is a function that accepts
    -- ValueChangeDumpDefinitions
    -- <$> unwraps the either monad so it can operate underneath it
    let waveform = (buildWaveValueMap . simulations) <$> definitions

    -- TODO: I'm currently grabbing these in the pad function as well
    let minTime = minimumTime <$> waveform
    let maxTime = maximumTime <$> waveform

    let decl = declarations <$> definitions

    -- >>= means apply a function that itself returns the same monadic type
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- You start with an `m a`, apply the `a -> m b` function to it to get a `m (m b)`
    -- (this is the map part) and then you flatten the `m (m b)` into an `m b`
    -- (this is the flat part).
    -- https://www.quora.com/What-do-the-symbols-and-mean-in-haskell
    let timeScale = decl >>= getTimescale

    return $ WaveConstruct <$> waveform <*> minTime <*> maxTime <*> timeScale

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

{- | Render a signal as display-width-safe glyphs for terminal UIs such as
Brick.  Unlike 'renderIdentifier', this deliberately omits ANSI escape
sequences: Brick measures those bytes as text even though the terminal does
not display them, causing the visible waveform to be clipped.
-}
renderPlainIdentifier :: IdentifierCode -> WaveConstruct -> String
renderPlainIdentifier ident wave =
    case HM.lookup ident (wWaveform wave) of
        Nothing -> "Signal Not Found"
        Just waveValues ->
            let resampled = resample (wMin wave) (wMax wave) waveValues
                renderTicks = toRenderTicks resampled
             in map (ccChar . tickToColored defaultColors) renderTicks
