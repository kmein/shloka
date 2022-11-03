module Shloka.Metre where

import Shloka.Syllable (Syllable(Syllable))
import Shloka.Token (longVowelTokens)

data Metre = Shloka | Trishtubh
    deriving (Show)

data Length = Laghu | Guna

instance Show Length where
    show Laghu = "L"
    show Guna = "G"
    showList x s = concat (map show x) ++ s

scanSyllable :: Syllable -> Length
scanSyllable (Syllable segments) =
    if length segments > 2
        then Guna
        else
            if last segments `elem` longVowelTokens
                then Guna
                else Laghu

guessMetre :: [[Length]] -> Maybe Metre
guessMetre verseParts
  | [a] <- verseParts, length a == 16 = Just Shloka
  | [a, b] <- verseParts, length a == 11 && length b == 11 = Just Trishtubh
  | otherwise = Nothing
