{-# LANGUAGE OverloadedStrings #-}

module TestUtil (
    goldenParserTest,
    goldenFileParserTest,
)
where

import Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Text.Pretty.Simple as PS

{- | Golden test for a parser whose input is supplied directly as Text.

Example:

goldenParserTest
   parseComment
   "simple comment"
   "test/golden/comment-simple.golden"
   "$comment hello world $end"
-}
goldenParserTest ::
    (Show a) =>
    Parser a ->
    TestName ->
    FilePath ->
    T.Text ->
    TestTree
goldenParserTest parser name goldenFile input =
    goldenVsString name goldenFile
        $ pure
            . BL.pack
            . show
        $ parseOnly parser input

{- | Golden test for a parser whose input comes from a file.

On success, the parsed result is pretty-printed.

Example:

goldenFileParserTest
   parseVCD
   "parses sample VCD"
   "test/vcd/sample.vcd"
   "test/golden/sample-vcd.golden"
-}
goldenFileParserTest ::
    (Show a) =>
    Parser a ->
    TestName ->
    FilePath ->
    FilePath ->
    TestTree
goldenFileParserTest parser name inputFile goldenFile =
    goldenVsString name goldenFile $ do
        input <- TIO.readFile inputFile

        pure . BL.pack $
            case parseOnly parser input of
                Left err ->
                    "Left " ++ show err
                Right result ->
                    TL.unpack $
                        PS.pShowNoColor result
