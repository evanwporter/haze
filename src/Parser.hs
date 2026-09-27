module Parser where

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
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
