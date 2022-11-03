{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shloka.Token where

import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec hiding (Token, token)

instance VisualStream [Token] where
    showTokens _ (t :| ts) = Text.unpack $ Text.intercalate "." (t : ts)

instance TraversableStream [Token] where
    -- https://hackage.haskell.org/package/megaparsec-9.2.1/docs/src/Text.Megaparsec.Stream.html#reachOffsetNoLine%27
    reachOffsetNoLine o PosState{..} =
        ( PosState
            { pstateInput = post
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = spos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = pstateLinePrefix
            }
        )
      where
        spos = case pstateSourcePos of
            (SourcePos n l c) -> SourcePos n l (c <> pos1)
        post = drop (o - pstateOffset) pstateInput

type Token = Text

data VowelLength = Long | Short
    deriving (Show)

data Category = Vowel VowelLength | Consonant
    deriving (Show)

-- Harvard Kyoto is defined here: https://web.archive.org/web/20210302180033/https://indology.info/email/members/wujastyk/#x1-120005.2

consonantTokens :: [Token]
consonantTokens = ["k", "kh", "g", "gh", "G"] ++ ["c", "ch", "j", "jh", "J"] ++ ["T", "Th", "D", "Dh", "N"] ++ ["t", "th", "d", "dh", "n"] ++ ["p", "ph", "b", "bh", "m"] ++ ["y", "r", "l", "v"] ++ ["z", "S", "s", "h"] ++ ["M", "H", "&", "f", "x"]
{-# NOINLINE consonantTokens #-}

shortVowelTokens :: [Token]
shortVowelTokens = ["a", "i", "u", "R", "L"]

longVowelTokens :: [Token]
longVowelTokens = ["A", "I", "U", "e", "o", "ai", "au", "q", "E"]

tokenToCategory :: Token -> Category
tokenToCategory token
    | token `elem` consonantTokens = Consonant
    | token `elem` shortVowelTokens = Vowel Short
    | token `elem` longVowelTokens = Vowel Long
    | otherwise = error $ "No such token: " ++ show token

tokenize :: Text -> [Token]
tokenize = go []
  where
    allTokens = sortOn (negate . Text.length) $ consonantTokens ++ longVowelTokens ++ shortVowelTokens
    matchingToken x = find (`Text.isPrefixOf` x) allTokens
    go acc word =
        case matchingToken word of
            Just token ->
                let (x, y) = Text.splitAt (Text.length token) word
                 in go (x : acc) y
            Nothing -> reverse acc
