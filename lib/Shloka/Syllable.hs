{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Shloka.Syllable where

import Data.Maybe (maybeToList)
import Data.Void (Void)
import Shloka.Token
import Shloka.Extra (debug)
import Text.Megaparsec (Parsec, eof, many, oneOf, optional, errorBundlePretty, parse, some, try, (<|>))

newtype Syllable = Syllable { segments :: [Token] }

instance Show Syllable where show = show . segments

tokenIsVowel :: Token -> Bool
tokenIsVowel x = x `elem` shortVowelTokens || x `elem` longVowelTokens

onset, rhyme, nucleus, coda :: Syllable -> [Token]
onset = takeWhile (not . tokenIsVowel) . segments
rhyme = dropWhile (not . tokenIsVowel) . segments
nucleus = takeWhile tokenIsVowel . rhyme
coda = dropWhile tokenIsVowel . rhyme

syllabify :: [Token] -> [Syllable]
syllabify toks = either (\e -> debug (errorBundlePretty e) []) (map Syllable) $ parse withInitialConsonant "" (reverse toks)
  where
    onHead :: (a -> a) -> [a] -> [a]
    onHead f = \case
        [] -> []
        (x : xs) -> f x : xs
    withInitialConsonant = do
        syls <- some (try syllable)
        initialConsonants <- many $ oneOf consonantTokens
        eof
        pure $ onHead (initialConsonants ++) (reverse syls)

-- parse in reverse: figure out syllables from right to left
syllable :: Parsec Void [Token] [Token]
syllable = do
    codaTokens <- many $ oneOf consonantTokens
    nucleusToken <- oneOf $ longVowelTokens <|> shortVowelTokens
    onsetToken <- optional $ oneOf consonantTokens
    return $ maybeToList onsetToken ++ [nucleusToken] ++ reverse codaTokens
