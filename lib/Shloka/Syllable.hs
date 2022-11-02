{-# LANGUAGE OverloadedStrings #-}

module Shloka.Syllable where

import Data.Maybe (maybeToList)
import Data.Void (Void)
import Shloka.Token
import Text.Megaparsec (Parsec, eof, many, oneOf, optional, parse, single, some, try, (<|>))
import qualified Text.Megaparsec (Token)
import qualified Text.Megaparsec.Stream

singles :: Text.Megaparsec.Stream.Stream s => [Text.Megaparsec.Token s] -> Parsec Void s [Text.Megaparsec.Token s]
singles xs = xs <$ mapM single xs

-- TODO we may want to throw away initial "s" here: skandhaH gets parsed as if it were kandhaH
-- this has no effect on the metre, as it is only thrown away if no words precedes it.
-- if a word precedes skandhaH, e. g. tasya skandhaH, it will be syllabified as tas.yas.kan.dhaH,
-- which is good enough for metrical purposes
syllabify :: [Token] -> [[Token]]
syllabify toks = either (error . show) id $ parse withInitialConsonant "" (reverse toks)
  where
    withInitialConsonant = do
        syls <- some (try syllable)
        initialConsonants <- many $ oneOf consonantTokens
        eof
        pure $ onHead (initialConsonants ++) (reverse syls)

onHead :: (a -> a) -> [a] -> [a]
onHead f = \case
    [] -> []
    (x : xs) -> f x : xs

-- parse in reverse: figure out syllables from right to left
syllable :: Parsec Void [Token] [Token]
syllable = do
    coda <- many $ oneOf consonantTokens
    nucleus <- oneOf $ longVowelTokens <|> shortVowelTokens
    onset <- optional $ oneOf consonantTokens
    return $ maybeToList onset ++ [nucleus] ++ reverse coda

{-

          try
              ( try (singles ["r", "p"])
                  <|> try (singles ["r", "z"])
                  <|> try (singles ["r", "b"])
                  <|> try (singles ["r", "t"])
                  <|> try (singles ["y", "v"])
                  <|> try (singles ["S", "k"])
                  <|> try (singles ["k", "s"])
                  <|> try (singles ["v", "s"])
                  <|> try (singles ["y", "n"])
                  <|> singles ["v", "d"]
              )
              <|> ((: []) <$> oneOf consonantTokens)
  -}
