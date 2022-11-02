module Main where

import qualified Data.Text as Text

import Control.Monad (forM_)
import Data.Either (rights)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text.IO as Text (readFile)
import Shloka.Parse (Line (..), NoLine, parse)
import Shloka.Syllable (syllabify)
import Shloka.Token (Token, longVowelTokens, tokenize)
import Text.Printf (printf)

syllabifyVerse :: Text -> [[Token]]
syllabifyVerse = syllabify . concatMap (tokenize . Text.replace "'" "") . Text.words

data Metre = Shloka | Trishtubh
    deriving (Show)

data Length = Long | Short

instance Show Length where
    show Long = "–"
    show Short = "⏑"
    showList x s = concat (map show x) ++ s

scanVerse :: Text -> ([Metre], [[Length]])
scanVerse verse
    | length verseParts == 1
        && length (head syllables) == 16 =
        ([Shloka], chunksOf 4 $ concat lengths)
    | length verseParts == 2
        && length (head syllables) == 11
        && length (head $ tail $ syllables) == 11 =
        ([Trishtubh], concatMap (chunksOf 4) lengths)
    | otherwise = ([], lengths)
  where
    verseParts = Text.splitOn "; " verse
    syllables = map syllabifyVerse verseParts
    lengths = map (map scanSyllable) syllables

scanSyllable :: [Token] -> Length
scanSyllable syllable =
    if length syllable > 2
        then Long
        else
            if last syllable `elem` longVowelTokens
                then Long
                else Short

mahabharataBook :: Int -> IO [Either NoLine Line]
mahabharataBook book =
    (either (error . show) id . parse) <$> Text.readFile (printf "text/MBh%02d.txt" book)

main :: IO ()
main = putStrLn "It compiles, ship it!"
