{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import qualified Data.Attoparsec.Text as A
-- import qualified Data.Text as T
import Types

parseValue :: A.Parser Value
parseValue =
  (A.char '0' *> pure V0)
    <|> (A.char '1' *> pure V1)
    <|> (A.char 'x' *> pure Vx)
    <|> (A.char 'X' *> pure VX)
    <|> (A.char 'z' *> pure Vz)
    <|> (A.char 'Z' *> pure VZ)
    A.<?> "value (0|1|x|z)" -- error message

parseTimeUnit :: A.Parser TimeUnit
parseTimeUnit =
  (A.string "fs" *> pure FemtoSeconds)
    <|> (A.string "ps" *> pure PicoSeconds)
    <|> (A.string "ns" *> pure NanoSeconds)
    <|> (A.string "us" *> pure MicroSeconds)
    <|> (A.string "ms" *> pure MilliSeconds)
    <|> (A.string "s" *> pure Seconds)
    A.<?> "time unit (s|ms|us|ns|ps|fs)"

parseComment :: A.Parser CommentText
parseComment = (A.string "$comment" *> A.skipSpace)
