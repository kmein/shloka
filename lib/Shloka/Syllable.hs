{-# LANGUAGE OverloadedStrings #-}

module Shloka.Syllable where

import Data.Maybe (maybeToList)
import Data.Void (Void)
import Shloka.Token
import Text.Megaparsec (Parsec, eof, many, oneOf, optional, parse, someTill, (<|>))

-- TODO we may want to throw away initial "s" here: skandhaH gets parsed as if it were kandhaH
-- this has no effect on the metre, as it is only thrown away if no words precedes it.
-- if a word precedes skandhaH, e. g. tasya skandhaH, it will be syllabified as tas.yas.kan.dhaH,
-- which is good enough for metrical purposes
syllabify :: [Token] -> [[Token]]
syllabify toks = case parse (syllable `someTill` eof) "" (reverse toks) of
    Right syls -> reverse syls
    Left e -> error $ show e

-- parse in reverse: figure out syllables from right to left
syllable :: Parsec Void [Token] [Token]
syllable = do
    coda <- many $ oneOf consonantTokens
    nucleus <- oneOf $ longVowelTokens <|> shortVowelTokens
    onset <- optional $ oneOf consonantTokens
    return $ maybeToList onset ++ [nucleus] ++ reverse coda
