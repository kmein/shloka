{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import Metai.Hexameter
import Metai.Parse
import Metai.Syllable
import Metai.Token
import Test.Tasty
import Test.Tasty.HUnit

tokenization :: TestTree
tokenization =
    testGroup
        "Tokenization"
        []

syllabification :: TestTree
syllabification =
    testGroup
        "Syllabification"
        []

scansion :: TestTree
scansion =
    testGroup
        "Scansion"
        []

main :: IO ()
main = defaultMain $ testGroup "Tests" [tokenization, syllabification, scansion]
