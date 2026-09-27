{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import qualified Data.Attoparsec.Text as A

import qualified Data.Text as T
import Types

-- The general pattern here is:
--
-- <|> means try parser on the left and if that fails
-- then try the parser on the right
--
-- \*> means if the left side is successfull then discard the result
-- and use the stuff on the right
--
-- <?> is a an Attoparsec operator for labeling the parser
-- its useful for error messages and the like
--
-- <$> is like doing fmap A B <-> A <$> B. The reason we use this
-- is to run A on the underlying value of B
--
-- . pipes the output of the right function into the left function.

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

parseTimeNumber :: A.Parser TimeNumber
parseTimeNumber =
    -- Parse in reverse since 10 could match 100 and 1
    -- could match 10 and 100
    (A.string "100" *> pure T100)
        <|> (A.string "10" *> pure T10)
        <|> (A.string "1" *> pure T1)
        A.<?> "time number (1|10|100)"

parseTimeScale :: A.Parser DeclarationCommand
parseTimeScale = do
    _ <- A.string "$timescale"
    A.skipSpace
    timeUnit <- parseTimeUnit
    timeNum <- parseTimeNumber
    pure (TimeScale timeNum timeUnit)

{- | Accept as input a keyword (Text) and a function that accepts Text and returns
a DeclarationCommand.
-}
parseDeclarationText :: T.Text -> (T.Text -> DeclarationCommand) -> A.Parser DeclarationCommand
parseDeclarationText keyword constructor =
    ( A.string keyword
        *> A.skipSpace
        -- A.takeTill is a parser that returns the string until it
        -- hits `$`. Then <$> applies CommentText to the underlying
        -- value that the parser holds which is `Text`
        *> (constructor <$> A.takeTill (== '$'))
        <* A.string "$end"
    )

parseComment :: A.Parser DeclarationCommand
parseComment = parseDeclarationText "$comment" (Comment . CommentText)

parseDate :: A.Parser DeclarationCommand
parseDate = parseDeclarationText "$date" (Date . DateText)
