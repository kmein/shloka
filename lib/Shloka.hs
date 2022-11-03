{-# LANGUAGE OverloadedStrings #-}

module Shloka where

import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Shloka.Metre (Length, Metre (..), guessMetre, scanSyllable)
import Shloka.Syllable (Syllable, syllabify)
import Shloka.Token (tokenize)

syllabifyVerse :: Text -> [Syllable]
syllabifyVerse = syllabify . concatMap (tokenize . Text.replace "'" "") . Text.words

scanVerse :: Text -> ([[Syllable]], [[Length]], Maybe Metre)
scanVerse verse =
    ( syllables
    , case metre of
        Just Shloka -> chunksOf 4 $ concat lengths
        Just Trishtubh -> concatMap (chunksOf 4) lengths
        Nothing -> lengths
    , metre
    )
  where
    verseParts = Text.splitOn "; " verse
    syllables = map syllabifyVerse verseParts
    lengths = map (map scanSyllable) syllables
    metre = guessMetre lengths
