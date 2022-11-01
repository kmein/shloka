module Main where

import qualified Data.Text as Text

import Data.List.Split (chunksOf)
import Shloka.Syllable (syllabify)
import Shloka.Token (longVowelTokens, tokenize)

syllabifyVerse = syllabify . tokenize . Text.concat . Text.words

data Verse = Shloka [[Length]] | Other [[Length]]
    deriving (Show)

data Length = Long | Short

instance Show Length where
    show Long = "–"
    show Short = "⏑"
    showList x s = concat (map show x) ++ s

scanVerse verse =
    let verseParts = Text.splitOn "; " verse
        syllables = map syllabifyVerse verseParts
        lengths = map (map scanSyllable) syllables
     in if length verseParts == 1 && length (head syllables) == 16
            then Shloka $ chunksOf 4 $ concat lengths
            else Other lengths

scanSyllable syllable =
    if length syllable > 2
        then Long
        else
            if last syllable `elem` longVowelTokens
                then Long
                else Short

main :: IO ()
main = putStrLn "It compiles, ship it!"
