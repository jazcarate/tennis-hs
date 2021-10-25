module Main where

import           Test.Hspec
import           Test.QuickCheck
import qualified TennisSpec

main :: IO ()
main = hspec TennisSpec.spec