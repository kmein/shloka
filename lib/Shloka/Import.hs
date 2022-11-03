{-# LANGUAGE LambdaCase #-}

module Shloka.Import where

import qualified Data.Text.IO as Text
import Shloka.Parse
import Text.Megaparsec (errorBundlePretty)
import Text.Printf (printf)

data Epic = Mahabharata | Ramayana
    deriving (Show)

type Kanda = [Either NoLine Line]

kandaCount :: Epic -> Int
kandaCount = \case
    Mahabharata -> 18
    Ramayana -> 7

readKanda :: Epic -> Int -> IO Kanda
readKanda epic kanda =
    (either (error . errorBundlePretty) id . parse)
        <$> Text.readFile
            ( case epic of
                Mahabharata -> printf "text/MBh%02d.txt" kanda
                Ramayana -> printf "text/Ram%02d.txt" kanda
            )
