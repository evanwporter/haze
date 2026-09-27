{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import qualified Data.Attoparsec.Text as A

import qualified Data.Attoparsec.Combinator as A
import Data.Char (isSpace)
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

parseVersion :: A.Parser DeclarationCommand
parseVersion = parseDeclarationText "$version" (Version . VersionText)

parseVarType :: A.Parser VarType
parseVarType =
    (A.string "event" *> pure Event)
        <|> (A.string "integer" *> pure Integer)
        <|> (A.string "parameter" *> pure Parameter)
        <|> (A.string "real" *> pure Real)
        <|> (A.string "realtime" *> pure Realtime)
        <|> (A.string "reg" *> pure Reg)
        <|> (A.string "supply0" *> pure Supply0)
        <|> (A.string "supply1" *> pure Supply1)
        <|> (A.string "time" *> pure Time)
        <|> (A.string "triand" *> pure Triand)
        <|> (A.string "trior" *> pure Trior)
        <|> (A.string "trireg" *> pure Trireg)
        <|> (A.string "tri0" *> pure Tri0)
        <|> (A.string "tri1" *> pure Tri1)
        <|> (A.string "tri" *> pure Tri)
        <|> (A.string "wand" *> pure Wand)
        <|> (A.string "wire" *> pure Wire)
        <|> (A.string "wor" *> pure Wor)
        A.<?> "var type"

parseSize :: A.Parser Size
-- <$> is used here to apply Size to the underlying value
-- of A.decimal (which is a integer)
parseSize = Size <$> A.decimal A.<?> "size type"

parseIdentifierCode :: A.Parser IdentifierCode
parseIdentifierCode =
    IdentifierCode
        -- We use the `.` since for each character `not . isSpace`
        -- is equivalent to `not (isSpace char)`
        -- The <$> here allows IdentifierCode to slide in and be applied
        -- the text that is wrapped by the takeWhile1
        <$> A.takeWhile1 (not . isSpace)
        A.<?> "identifier code"

parseIndex :: A.Parser Index
parseIndex = Index <$> A.decimal

parseReference :: A.Parser Reference
parseReference =
    do
        name <- A.takeWhile1 (\c -> (not $ isSpace c) && (c /= '['))
        sel <- A.peekChar
        case sel of
            Just '[' -> do
                _ <- A.char '['
                msb <- parseIndex
                next <- A.peekChar
                case next of
                    Just ':' -> do
                        _ <- A.char ':'
                        lsb <- parseIndex
                        _ <- A.char ']'
                        return $ RangeSelect name msb lsb
                    Just ']' -> do
                        _ <- A.char ']'
                        return $ BitSelect name msb
                    _ ->
                        fail "expected ':' or ']'"
            _ -> return $ Identifier name
        A.<?> "reference"

parseVar :: A.Parser DeclarationCommand
parseVar = do
    A.string "$var" *> A.skipSpace
    varType <- parseVarType <* A.skipSpace
    varSize <- parseSize <* A.skipSpace
    varIDCode <- parseIdentifierCode <* A.skipSpace
    varReference <- parseReference <* A.skipSpace
    _ <- A.string "$end"
    return $ Var varType varSize varIDCode varReference

parseScopeType :: A.Parser ScopeType
parseScopeType =
    (A.string "begin" *> pure Begin)
        <|> (A.string "fork" *> pure Fork)
        <|> (A.string "function" *> pure Function)
        <|> (A.string "module" *> pure Module)
        <|> (A.string "task" *> pure Task)
        A.<?> "scope type"

-- TODO: Merge with identifier code
parseScopeIdentifier :: A.Parser ScopeIdentifier
parseScopeIdentifier =
    ScopeIdentifier
        <$> A.takeWhile1 (not . isSpace)
        A.<?> "scope indentifier"

parseScope :: A.Parser DeclarationCommand
parseScope = do
    A.string "$scope" *> A.skipSpace
    scopeType <- parseScopeType <* A.skipSpace
    scopeID <- parseScopeIdentifier <* A.skipSpace
    _ <- A.string "$end"
    return $ Scope scopeType scopeID

parseUpScope :: A.Parser DeclarationCommand
parseUpScope = do
    A.string "$upscope" *> A.skipSpace <* A.string "$end"
    return Upscope

parseEndDefinitions :: A.Parser DeclarationCommand
parseEndDefinitions = do
    A.string "$enddefinitions" *> A.skipSpace <* A.string "$end"
    return EndDefinitions

parseDeclarationCommand :: A.Parser DeclarationCommand
parseDeclarationCommand = do
    token <- A.lookAhead $ A.takeWhile1 (not . isSpace)
    case token of
        -- We don't need to prefix each parseX with return
        -- because each parseX already returns
        "$comment" -> parseComment
        "$date" -> parseDate
        "$enddefinitions" -> parseEndDefinitions
        "$scope" -> parseScope
        "$timescale" -> parseTimeScale
        "$upscope" -> parseUpScope
        "$var" -> parseVar
        "$version" -> parseVersion
        _ -> fail "unknown declaration command"

parseDeclarationCommands :: A.Parser [DeclarationCommand]
parseDeclarationCommands = do
    declaration <- parseDeclarationCommand
    case declaration of
        EndDefinitions -> return [declaration]
        _ -> do
            declarationCmds <- parseDeclarationCommands
            return (declaration : declarationCmds)

parseScalarValueChange :: A.Parser ScalarValueChange
parseScalarValueChange = do
    val <- parseValue
    idCode <- parseIdentifierCode
    return $ ScalarValueChange val idCode

parseVectorValueChange :: A.Parser VectorValueChange
parseVectorValueChange = do
    c <- A.anyChar
    case c of
        'b' -> do
            bin <- A.takeWhile1 (not . isSpace) <* A.skipSpace
            idCode <- parseIdentifierCode
            pure $ BinaryLower bin idCode
        'B' -> do
            bin <- A.takeWhile1 (not . isSpace) <* A.skipSpace
            idCode <- parseIdentifierCode
            pure $ BinaryUpper bin idCode
        'r' -> do
            num <- A.double <* A.skipSpace
            idCode <- parseIdentifierCode
            pure $ RealLower num idCode
        'R' -> do
            num <- A.double <* A.skipSpace
            idCode <- parseIdentifierCode
            pure $ RealUpper num idCode
        _ ->
            fail "expected vector value change (b|B|r|R)"

parseValueChange :: A.Parser ValueChange
parseValueChange = do
    c <- A.peekChar'

    if
        | c `elem` ['0', '1', 'x', 'X', 'z', 'Z'] ->
            ScalarChange <$> parseScalarValueChange
        | c `elem` ['b', 'B', 'r', 'R'] ->
            VectorChange <$> parseVectorValueChange
        | otherwise ->
            fail "unknown value change prefix"

parseValueChanges :: A.Parser [ValueChange]
parseValueChanges = do
    val <- parseValueChange <* A.skipSpace
    c <- A.peekChar
    case c of
        Just '$' -> return [val]
        _ -> do
            rest <- parseValueChanges
            return $ val : rest

parseSimulationKeyword :: A.Parser SimulationCommand
parseSimulationKeyword = do
    token <- A.takeWhile1 (not . isSpace)
    A.skipSpace

    case token of
        "$dumpall" -> do
            -- TODO Skip space here might not be necessary
            vals <- parseValueChanges <* A.skipSpace
            A.string "$end" *> A.skipSpace
            pure $ DumpAll vals
        "$dumpoff" -> do
            vals <- parseValueChanges <* A.skipSpace
            A.string "$end" *> A.skipSpace
            pure $ DumpOff vals
        "$dumpon" -> do
            vals <- parseValueChanges <* A.skipSpace
            A.string "$end" *> A.skipSpace
            pure $ DumpOn vals
        "$dumpvars" -> do
            vals <- parseValueChanges <* A.skipSpace
            A.string "$end" *> A.skipSpace
            pure $ DumpVars vals
        "$comment" -> do
            comment <- CommentText <$> A.takeTill (== '$')
            A.string "$end" *> A.skipSpace
            pure $ SimComment comment
        _ ->
            fail "unknown simulation keyword"

parseSimulationTime :: A.Parser SimulationCommand
parseSimulationTime = do
    _ <- A.char '#'
    num <- A.decimal
    return $ SimTime (SimulationTime num)

parseSimulationCommand :: A.Parser SimulationCommand
parseSimulationCommand = do
    c <- A.peekChar'

    if
        | c `elem` ['0', '1', 'x', 'X', 'z', 'Z', 'b', 'B', 'r', 'R'] ->
            -- We don't return here because this is the exact type that
            -- we need. The SimValueChange <$> applies the SimValueChange
            -- to the thing underneath A.Parser
            SimValueChange <$> parseValueChange
        | c `elem` ['$'] ->
            parseSimulationKeyword
        | c `elem` ['#'] ->
            parseSimulationTime
        | otherwise ->
            fail "unknown simulation command"
