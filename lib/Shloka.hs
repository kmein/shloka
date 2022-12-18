{-# LANGUAGE OverloadedStrings #-}

module Shloka where

import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Shloka.Metre (Length, Metre (..), guessMetre, scanSyllable)
import Shloka.Syllable (Syllable, syllabify)
import Shloka.Token (tokenize)

syllabifyVerse :: Text -> [Syllable]
syllabifyVerse = syllabify . tokenize . Text.replace "'" ""

scanVerse :: Text -> ([[Syllable]], [[(Length, Int)]])
scanVerse verse =
    ( syllables
    , lengths
    )
  where
    verseParts = Text.splitOn "; " verse
    syllables = map syllabifyVerse verseParts
    lengths = map (map scanSyllable) syllables
