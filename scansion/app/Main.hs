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
import Shloka (
    Epic (Mahabharata, Ramayana),
    Line (..),
    categorySymbol,
    readStdin,
    renderLengthWithBreak,
    scanVerse,
    tokenToCategory,
 )
import System.Environment (lookupEnv)

analyse :: Line -> Map Text Text
analyse l =
    let (parva, adhyaya, shloka, pada) = lineLocation l
        (verseParts, lengths) = scanVerse $ lineText l
     in [ ("parvan", pack $ show parva)
        , ("adhyaya", pack $ show adhyaya)
        , ("shloka", pack $ show shloka)
        , ("pada", maybe Text.empty Text.singleton pada)
        , ("type", pack $ show $ lineType l)
        , ("text", lineText l)
        ,
            ( "syllables"
            , Text.intercalate "/" $
                map
                    ( Text.intercalate "."
                        . map (Text.concat . (\(v, cs) -> v : cs))
                    )
                    verseParts
            )
        ,
            ( "syllables_symbols"
            , Text.intercalate "/" $
                map
                    ( Text.intercalate "."
                        . map
                            ( \(v, cs) ->
                                Text.pack $
                                    map (categorySymbol . tokenToCategory) $ v : cs
                            )
                    )
                    verseParts
            )
        ,
            ( "lengths"
            , Text.intercalate "; " $
                map (Text.concat . map renderLengthWithBreak) lengths
            )
        ]

csvColumns :: Header
csvColumns =
    [ "parvan"
    , "adhyaya"
    , "shloka"
    , "pada"
    , "type"
    , "text"
    , "syllables"
    , "syllables_symbols"
    , "lengths"
    ]

main :: IO ()
main = do
    epicString <- lookupEnv "EPIC"
    let epic = case epicString of
          Just "mahabharata" -> Mahabharata
          Just "ramayana" -> Ramayana
          _ -> error "Please provide info on which epic you want to analyse in the EPIC environment variable."
    ByteString.putStr . encodeByName csvColumns =<< (fmap (map analyse . rights) $ readStdin epic)
