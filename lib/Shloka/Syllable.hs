{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Shloka.Syllable where

import Data.Maybe (maybeToList)
import Data.Void (Void)
import Shloka.Extra (debug)
import Shloka.Token
import Text.Megaparsec (Parsec, eof, errorBundlePretty, many, oneOf, optional, parse, some, try, (<|>))

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
                [nucleus1, nucleus2] -> (nucleus1, []) : (nucleus2, consonants) : syllabify rest'
                -- 01,114.038a tathā devaṛṣīṇāṃ
                _ -> error $ show str
