module Shloka.Metre where

import Shloka.Syllable (Syllable, coda, nucleus)
import Shloka.Token (longVowelTokens)

data Metre = Shloka | Trishtubh
    deriving (Eq, Show)

data Length = Laghu | Guru

instance Show Length where
    show Laghu = "L"
    show Guru = "G"
    showList x s = concat (map show x) ++ s

scanSyllable :: Syllable -> Length
scanSyllable syllable =
    if length (coda syllable) > 0
        then Guru
        else
            if all (`elem` longVowelTokens) $ nucleus syllable
                then Guru
                else Laghu

guessMetre :: [[Length]] -> Maybe Metre
guessMetre verseParts
    | [a] <- verseParts, length a == 16 = Just Shloka
    | [a, b] <- verseParts, length a == 11 && length b == 11 = Just Trishtubh
    | otherwise = Nothing
