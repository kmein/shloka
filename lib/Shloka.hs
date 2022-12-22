{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shloka where

import Data.Char
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void)
import Text.Megaparsec hiding (Token, parse, parseTest)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char (char, crlf, digitChar, letterChar, string)
import Text.Printf (printf)

---
--- IMPORT
---

data Epic = Mahabharata | Ramayana
    deriving (Show)

type Kanda = [Either NoLine Line]

kandaCount :: Epic -> Int
kandaCount = \case
    Mahabharata -> 18
    Ramayana -> 7

readKanda :: Epic -> Int -> IO Kanda
readKanda epic kanda =
    (either (error . errorBundlePretty) id . parse)
        <$> Text.readFile
            ( case epic of
                Mahabharata -> printf "text/MBh%02d.txt" kanda
                Ramayana -> printf "text/Ram%02d.txt" kanda
            )

---
--- PARSE
---

type Parser = Parsec Void Text.Text

data NoLine = Comment Text
    deriving (Show)

data LineType = Verse | Prose | Heading
    deriving (Show, Eq)

data Line = Line
    { lineLocation :: (Int, Int, Int, Maybe Char)
    , lineType :: LineType
    , lineText :: Text
    }
    deriving (Show)

parseTest :: Text -> IO ()
parseTest = Megaparsec.parseTest theParser

parse :: Text -> Either (ParseErrorBundle Text Void) [Either NoLine Line]
parse = Megaparsec.parse theParser ""

theParser :: Parser [Either NoLine Line]
theParser =
    ( ( (Right <$> parseLine)
            <|> (Left <$> parseComment)
      )
        `sepEndBy` crlf
    )
        <* eof

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
    let lineLocation =
            ( location1
            , location2
            , location3
            , if lineTypeChar == ' ' then Nothing else Just lineTypeChar
            )
    _ <- char ' '
    lineText <- Text.pack <$> many harvardKyoto
    pure $ Line{..}
  where
    harvardKyoto =
        letterChar
            <|> oneOf (" '-;&:" :: [Char])
            <|> (' ' <$ (digitChar <|> oneOf ("[]=" :: [Char])))

instance VisualStream [Token] where
    showTokens _ (t :| ts) = Text.unpack $ Text.intercalate "." (t : ts)

instance TraversableStream [Token] where
    -- https://hackage.haskell.org/package/megaparsec-9.2.1/docs/src/Text.Megaparsec.Stream.html#reachOffsetNoLine%27
    reachOffsetNoLine o PosState{..} =
        ( PosState
            { pstateInput = post
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = spos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = pstateLinePrefix
            }
        )
      where
        spos = case pstateSourcePos of
            (SourcePos n l c) -> SourcePos n l (c <> pos1)
        post = drop (o - pstateOffset) pstateInput

---
--- TOKENIZE
---

type Token = Text

data VowelLength = Long | Short
    deriving (Show)

data Category = Vowel VowelLength | Consonant
    deriving (Show)

consonantTokens :: [Token]
consonantTokens =
    ["k", "kh", "g", "gh", "G"]
        ++ ["c", "ch", "j", "jh", "J"]
        ++ ["T", "Th", "D", "Dh", "N"]
        ++ ["t", "th", "d", "dh", "n"]
        ++ ["p", "ph", "b", "bh", "m"]
        ++ ["y", "r", "l", "v"]
        ++ ["z", "S", "s", "h"]
        ++ ["M", "H", "&", "f", "x"]
{-# NOINLINE consonantTokens #-}

shortVowelTokens :: [Token]
shortVowelTokens = ["a", "i", "u", "R", "L"]

longVowelTokens :: [Token]
longVowelTokens = ["A", "I", "U", "e", "o", "ai", "au", "q", "E"]

wordSeparatorTokens :: [Token]
wordSeparatorTokens = [" ", ":", "-"]

tokenToCategory :: Token -> Category
tokenToCategory t
    | t `elem` consonantTokens = Consonant
    | t `elem` shortVowelTokens = Vowel Short
    | t `elem` longVowelTokens = Vowel Long
    | otherwise = error $ "No such token: " ++ show t

tokenize :: Text -> [Token]
tokenize = go []
  where
    allTokens =
        sortOn (negate . Text.length) $
            concat
                [ consonantTokens
                , longVowelTokens
                , shortVowelTokens
                , wordSeparatorTokens
                ]
    matchingToken x = find (`Text.isPrefixOf` x) allTokens
    go acc word =
        case matchingToken word of
            Just t ->
                let (x, y) = Text.splitAt (Text.length t) word
                 in go (x : acc) y
            Nothing -> reverse acc

---
--- SYLLABIFY
---

type Syllable = (Token, [Token]) -- nucleus and following consonants

tokenIsVowel :: Token -> Bool
tokenIsVowel x = x `elem` shortVowelTokens || x `elem` longVowelTokens

syllabify :: [Token] -> [Syllable]
syllabify str
    | null str = []
    | otherwise =
        let vowelInitialSubstring = dropWhile (not . tokenIsVowel) str
            (vowels, rest) = span tokenIsVowel vowelInitialSubstring
            (consonants, rest') = span (not . tokenIsVowel) rest
         in case vowels of
                [nucleus] -> (nucleus, consonants) : syllabify rest'
                [nucleus1, nucleus2] ->
                    (nucleus1, []) : (nucleus2, consonants) : syllabify rest'
                -- 01,114.038a tathā devaṛṣīṇāṃ
                _ -> error $ show str

---
--- SCAN
---

data Length = Laghu | Guru

instance Show Length where
    show Laghu = "L"
    show Guru = "G"
    showList x s = concat (map show x) ++ s

renderLengthWithBreak :: (Length, Int) -> Text
renderLengthWithBreak (l, breaks) =
    pack $
        ( case l of
            Laghu -> "L"
            Guru -> "G"
        )
            ++ replicate breaks '.'

scanSyllable :: Syllable -> (Length, Int)
scanSyllable (vowel, coda) = (matra, wordEnd)
  where
    cleanCoda = filter (`notElem` wordSeparatorTokens) coda
    matra =
        if vowel `elem` longVowelTokens || length cleanCoda > 1 || "ch" `elem` cleanCoda
            then Guru
            else Laghu
    wordEnd = length $ filter (== " ") coda

scanVerse :: Text -> ([[Syllable]], [[(Length, Int)]])
scanVerse verse = (syllables, lengths)
  where
    lengths = map (map scanSyllable) syllables
    syllables = map syllabifyVerse verseParts
      where
        verseParts = Text.splitOn "; " verse
        syllabifyVerse = syllabify . tokenize . Text.replace "'" ""
