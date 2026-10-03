module Haze where

import qualified Data.Text.IO as TIO
import Parser (parseText)
import Types
import Waveform (Waveform, buildWaveform)

-- main = do
--   contents <- TIO.readFile "file.txt"

parseVCDFile :: FilePath -> IO (Either String ValueChangeDumpDefinitions)
parseVCDFile path = do
  content <- TIO.readFile path
  return $ parseText content

haze :: FilePath -> IO (Either String Waveform)
haze path = do
  -- do unwraps the IO monad but we still need to handle
  -- the Either monad
  definitions <- parseVCDFile path

  -- (buildWaveform . simulations) is a function that accepts
  -- ValueChangeDumpDefinitions
  -- <$> unwraps definitions so it can operate underneath it
  return $ (buildWaveform . simulations) <$> definitions

-- contents is Data.Text.Text
