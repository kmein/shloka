{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text as Text

import Control.Monad (forM)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.Either (rights)
import Data.Map (Map)
import Data.Text (Text, pack)
import Shloka
import Shloka.Import
import Shloka.Metre (renderLengthWithBreak)
import Shloka.Parse (Line (..))
import Shloka.Syllable (Syllable)

analyse :: Line -> Map Text Text
analyse l =
    let (parva, subParva, verse, subVerse) = lineLocation l
        (verseParts, lengths) = scanVerse $ lineText l
     in [ ("parva", pack $ show parva)
        , ("sub_parva", pack $ show subParva)
        , ("verse", pack $ show verse)
        , ("sub_verse", maybe Text.empty Text.singleton subVerse)
        , ("type", pack $ show $ lineType l)
        , ("text", lineText l)
        , ("syllables", Text.intercalate "/" $ map (Text.intercalate "." . map (Text.concat . (\(v, cs) -> v : cs))) verseParts)
        , ("lengths", Text.intercalate "; " $ map (Text.concat . map renderLengthWithBreak) lengths)
        ]

csvColumns :: Header
csvColumns = ["parva", "sub_parva", "verse", "sub_verse", "type", "text", "syllables", "lengths"]

main :: IO ()
main =
    ByteString.putStr . encodeByName csvColumns . concat
        =<< forM @[] [1 .. kandaCount epic] (fmap (map analyse . rights) . readKanda epic)
  where
    epic = Mahabharata
