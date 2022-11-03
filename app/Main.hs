{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Text as Text

import Control.Monad (forM_)
import Data.Either (rights)
import Data.Text (Text, pack)
import Shloka.Parse (Line (..))
import Shloka.Syllable (Syllable(segments))
import Data.Csv
import qualified Data.ByteString.Lazy as ByteString
import Data.Map (Map)
import Shloka.Import
import Shloka

analyse :: Line -> Map Text Text
analyse l =
  let
    (parva, subParva, verse, subVerse) = lineLocation l
    (verseParts, lengths, metre) = scanVerse $ lineText l
  in
  [ ("parva", pack $ show parva )
  , ("sub_parva", pack $ show subParva)
  , ("verse", pack $ show verse)
  , ("sub_verse", maybe Text.empty Text.singleton subVerse)
  , ("type", pack $ show $ lineType l)
  , ("metre", maybe Text.empty (pack.show) metre)
  , ("text", lineText l)
  , ("syllables", Text.intercalate "/" $ map (Text.intercalate "." . map (Text.concat . segments)) verseParts)
  , ("lengths", Text.intercalate "."$ map (pack.show) lengths)
  ]

csvColumns :: Header
csvColumns = ["parva", "sub_parva", "verse", "sub_verse", "type", "metre", "text", "syllables", "lengths"]

main :: IO ()
main =
    forM_ @[] [Mahabharata] $ \epic ->
      forM_ @[] [1 .. kandaCount epic] $ \kanda -> do
          kandaLines <- rights <$> readKanda epic kanda
          ByteString.putStr $ encodeByName csvColumns $ map analyse kandaLines
