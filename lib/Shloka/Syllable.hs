{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Shloka.Syllable where

import Data.Maybe (maybeToList)
import Data.Void (Void)
import Shloka.Token
import Text.Megaparsec (Parsec, eof, many, oneOf, optional, parse, some, try, (<|>))

newtype Syllable = Syllable { segments :: [Token] }

syllabify :: [Token] -> [Syllable]
syllabify toks = either (error . show) (map Syllable) $ parse withInitialConsonant "" (reverse toks)
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
    coda <- many $ oneOf consonantTokens
    nucleus <- oneOf $ longVowelTokens <|> shortVowelTokens
    onset <- optional $ oneOf consonantTokens
    return $ maybeToList onset ++ [nucleus] ++ reverse coda
