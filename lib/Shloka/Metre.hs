{-# LANGUAGE OverloadedStrings #-}

module Shloka.Metre where

import Data.Text (Text, pack)
import Shloka.Syllable (Syllable)
import Shloka.Token (longVowelTokens)

data Metre = Shloka | Trishtubh
    deriving (Eq, Show)

data Length = Laghu | Guru

data WordBreak = WordBreak

instance Show Length where
    show Laghu = "L"
    show Guru = "G"
    showList x s = concat (map show x) ++ s

renderLengthWithBreak :: (Length, Maybe WordBreak) -> Text
renderLengthWithBreak (l, b) =
    pack $
        ( case l of
            Laghu -> "L"
            Guru -> "G"
        )
            ++ maybe "" (const ".") b

scanSyllable :: Syllable -> (Length, Maybe WordBreak)
scanSyllable (vowel, coda) = (matra, wordEnd)
  where
    cleanCoda = filter (/= " ") coda
    matra =
        if vowel `elem` longVowelTokens || length cleanCoda > 1
            then Guru
            else Laghu
    wordEnd = if " " `elem` coda then Just WordBreak else Nothing

guessMetre :: [[Length]] -> Maybe Metre
guessMetre verseParts
    | [a] <- verseParts, length a == 16 = Just Shloka
    | [a, b] <- verseParts, length a == 11 && length b == 11 = Just Trishtubh
    | otherwise = Nothing
