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

data LineType = Verse Char | Prose Char | Heading
    deriving (Show)

data Line = Line {lineLocation :: (Int, Int, Int), lineType :: LineType, lineText :: Text}
    deriving (Show)

parse :: Parser [Either NoLine Line]
parse = (((Right <$> parseLine) <|> (Left <$> parseComment)) `sepEndBy` crlf) <* eof

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
