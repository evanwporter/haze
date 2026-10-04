module Haze (
    module Haze,
    module Parser,
    module Types,
    module Waveform,
)
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import Parser
import Types
import Waveform

{- | This is the primary output type for the Haze library. Ideally
everything that is needed to display the waveform should be contained
in this data block.
-}
data WaveConstruct = WaveConstruct
    { wWaveform :: WaveValueMap
    , wMin :: SimulationTime
    , wMax :: SimulationTime
    , wcTimescale :: (TimeNumber, TimeUnit)
    , wcSymbolMap :: HM.HashMap IdentifierCode Reference
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

buildSymbolMap :: [DeclarationCommand] -> HM.HashMap IdentifierCode Reference
buildSymbolMap decls = buildMapHelper decls HM.empty
  where
    buildMapHelper ::
        [DeclarationCommand] ->
        HM.HashMap IdentifierCode Reference ->
        HM.HashMap IdentifierCode Reference

    -- if the list is empty then there's no more stuff to parse to we
    -- return the input hash map
    buildMapHelper [] hm = hm
    buildMapHelper (decl : rest) hm = case decl of
        Var _ _ ident ref -> buildMapHelper rest (HM.insert ident ref hm)
        _ -> buildMapHelper rest hm

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

    let symbolMap = buildSymbolMap <$> decl

    return $ WaveConstruct <$> waveform <*> minTime <*> maxTime <*> timeScale <*> symbolMap
