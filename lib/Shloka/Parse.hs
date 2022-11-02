{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shloka.Parse where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, crlf, digitChar, letterChar, string)

type Parser = Parsec Void Text.Text

data NoLine = Comment Text
    deriving (Show)

data LineType = Verse | Prose | Heading
    deriving (Show)

data Line = Line {lineLocation :: (Int, Int, Int, Maybe Char), lineType :: LineType, lineText :: Text}
    deriving (Show)

parseTest :: Text -> IO ()
parseTest = Text.Megaparsec.parseTest theParser

parse :: Text -> Either (ParseErrorBundle Text Void) [Either NoLine Line]
parse = Text.Megaparsec.parse theParser ""

theParser :: Parser [Either NoLine Line]
theParser = (((Right <$> parseLine) <|> (Left <$> parseComment)) `sepEndBy` crlf) <* eof

parseComment :: Parser NoLine
parseComment =
    Comment . Text.pack
        <$> (string "%" *> optional (char ' ') *> (many (noneOf ['\r', '\n'])))

parseLine :: Parser Line
parseLine = do
    location1 <- read <$> count 2 digitChar
    location2 <- read <$> count 3 digitChar
    location3 <- read <$> count 3 digitChar
    lineTypeChar <- letterChar <|> char ' '
    lineType <- case lineTypeChar of
        ' ' -> pure Heading
        c
            | isLower c -> pure Verse
            | isUpper c -> pure Prose
            | otherwise -> fail $ "Unexpected line type: " ++ show c
    let lineLocation = (location1, location2, location3, if lineTypeChar == ' ' then Nothing else Just lineTypeChar)
    _ <- char ' '
    lineText <- Text.pack <$> many harvardKyoto
    pure $ Line{..}
  where
    harvardKyoto =
        letterChar
            <|> oneOf (" '-;&:" :: [Char])
            <|> (' ' <$ (digitChar <|> oneOf ("[]=" :: [Char])))
