{-# LANGUAGE LambdaCase #-}
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
import Shloka.Metre (Metre (Shloka), scanSyllable)
import Shloka.Parse (Line (..), LineType (Verse))
import Shloka.Syllable (syllabify)
import Shloka.Token (tokenize)

csvColumns :: Header
csvColumns = ["parva", "sub_parva", "verse", "sub_verse", "word", "lengths", "position"]

localiseWords :: Line -> [Map Text Text]
localiseWords line = go 1 [] $ Text.words $ lineText line
  where
    go currentPosition acc = \case
        [] -> reverse acc
        word : restWords ->
            let syllables = syllabify $ tokenize $ Text.replace "'" "" word
                (parva, subParva, verse, subVerse) = lineLocation line
             in go
                    (currentPosition + length syllables)
                    ( ( [ ("parva", pack $ show parva)
                        , ("sub_parva", pack $ show subParva)
                        , ("verse", pack $ show verse)
                        , ("sub_verse", maybe Text.empty Text.singleton subVerse)
                        , ("word", word)
                        , ("lengths", pack $ show $ map scanSyllable syllables)
                        , ("position", pack $ show currentPosition)
                        ] ::
                            Map Text Text
                      ) :
                      acc
                    )
                    restWords

main :: IO ()
main =
    ByteString.putStr . encodeByName csvColumns . concat
        =<< forM @[]
            [1 .. kandaCount epic]
            ( fmap (concatMap localiseWords . filter isShloka . rights) . readKanda epic
            )
  where
    epic = Mahabharata
    isShloka line = lineType line == Verse && metre == Just Shloka
      where
        (_, _, metre) = scanVerse $ lineText line
