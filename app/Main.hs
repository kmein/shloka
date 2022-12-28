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
    Epic (Mahabharata),
    Line (..),
    categorySymbol,
    kandaCount,
    readKanda,
    renderLengthWithBreak,
    scanVerse,
    tokenToCategory,
 )

analyse :: Line -> Map Text Text
analyse l =
    let (parva, subParva, verse, subVerse) = lineLocation l
        (verseParts, lengths) = scanVerse $ lineText l
     in [ ("parvan", pack $ show parva)
        , ("adhyaya", pack $ show subParva)
        , ("shloka", pack $ show verse)
        , ("pada", maybe Text.empty Text.singleton subVerse)
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
                        . map (\(v, cs) -> Text.pack $ map (categorySymbol . tokenToCategory) $ v : cs)
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
main =
    ByteString.putStr . encodeByName csvColumns . concat
        =<< forM @[]
            [1 .. kandaCount epic]
            (fmap (map analyse . rights) . readKanda epic)
  where
    epic = Mahabharata
