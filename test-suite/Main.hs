module Main where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Runners.AntXML

import NDP.Tests

main :: IO ()
main = defaultMainWithIngredients (antXMLRunner : defaultIngredients) ndpTests
