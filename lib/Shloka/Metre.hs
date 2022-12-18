{-# LANGUAGE OverloadedStrings #-}

module Shloka.Metre where

import Data.Text (Text, pack)
import Shloka.Syllable (Syllable)
import Shloka.Token (longVowelTokens, wordSeparatorTokens)

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
        if vowel `elem` longVowelTokens || length cleanCoda > 1
            then Guru
            else Laghu
    wordEnd = length $ filter (== " ") coda
