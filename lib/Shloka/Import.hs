{-# LANGUAGE LambdaCase #-}
module Shloka.Import where

import Shloka.Parse
import Text.Printf (printf)
import qualified Data.Text.IO as Text

data Epic = Mahabharata | Ramayana
  deriving Show

type Kanda = [Either NoLine Line]

kandaCount :: Epic -> Int
kandaCount = \case
  Mahabharata -> 18
  Ramayana -> 7

readKanda :: Epic -> Int -> IO Kanda
readKanda epic kanda =
  (either (error . show) id . parse) <$> Text.readFile (case epic of
    Mahabharata -> printf "text/MBh%02d.txt" kanda
    Ramayana -> printf "text/Ram%02d.txt" kanda)
