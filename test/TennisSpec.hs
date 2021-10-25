module TennisSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.List                     as L

data Point = P1 | P2 deriving (Eq)

data Game = Game [Point]

mkGame :: Game
mkGame = Game mempty

point :: Game -> Point -> Game
point (Game ps) p = Game (p : ps)

score :: Game -> String
score (Game points) = if ps1 == 4
  then "Game P1"
  else if ps2 == 4
    then "Game P2"
    else (scoreText ps1) <> " - " <> (scoreText ps2)
 where
  (ps1', ps2') = L.partition (== P1) points
  ps1          = length ps1'
  ps2          = length ps2'

scoreText :: Int -> String
scoreText 0 = "love"
scoreText 1 = "15"
scoreText 2 = "30"
scoreText 3 = "40"
scoreText _ = undefined


shouldScore :: [Point] -> String -> Expectation
shouldScore ps expected = (score $ foldl point mkGame ps) `shouldBe` expected


spec :: Spec
spec = do
  describe "scoring" $ do
    describe "x - love" $ do
      it "starts with love" $ do
        [] `shouldScore` "love - love"
      it "a point is 15" $ do
        [P1] `shouldScore` "15 - love"
      it "two point is 30" $ do
        [P1, P1] `shouldScore` "30 - love"
      it "three point is 40" $ do
        [P1, P1, P1] `shouldScore` "40 - love"
      it "four point is game" $ do
        [P1, P1, P1, P1] `shouldScore` "Game P1"
    describe "love - x" $ do
      let game = mkGame
      it "a point is 15" $ do
        [P2] `shouldScore` "love - 15"
      it "two point is 30" $ do
        [P2, P2] `shouldScore` "love - 30"
      it "three point is 40" $ do
        [P2, P2, P2] `shouldScore` "love - 40"
      it "four point is game" $ do
        [P2, P2, P2, P2] `shouldScore` "Game P2"
    describe "x - 15" $ do
      let initial = [P2]
      it "starts with 15" $ do
        initial `shouldScore` "love - 15"
      it "a point is 15" $ do
        (initial ++ [P1]) `shouldScore` "15 - 15"
      it "two point is 30" $ do
        (initial ++ [P1, P1]) `shouldScore` "30 - 15"
      it "three point is 40" $ do
        (initial ++ [P1, P1, P1]) `shouldScore` "40 - 15"
      it "four point is game" $ do
        (initial ++ [P1, P1, P1, P1]) `shouldScore` "Game P1"
    describe "15 - x" $ do
      let initial = [P1]
      it "starts with 15" $ do
        initial `shouldScore` "15 - love"
      it "a point is 15" $ do
        (initial ++ [P2]) `shouldScore` "15 - 15"
      it "two point is 30" $ do
        (initial ++ [P2, P2]) `shouldScore` "15 - 30"
      it "three point is 40" $ do
        (initial ++ [P2, P2, P2]) `shouldScore` "15 - 40"
      it "four point is game" $ do
        (initial ++ [P2, P2, P2, P2]) `shouldScore` "Game P2"
    describe "x - 30" $ do
      let initial = [P2, P2]
      it "starts with 30" $ do
        initial `shouldScore` "love - 30"
      it "a point is 15" $ do
        (initial ++ [P1]) `shouldScore` "15 - 30"
      it "two point is 30" $ do
        (initial ++ [P1, P1]) `shouldScore` "30 - 30"
      it "three point is 40" $ do
        (initial ++ [P1, P1, P1]) `shouldScore` "40 - 30"
      it "four point is game" $ do
        (initial ++ [P1, P1, P1, P1]) `shouldScore` "Game P1"
    describe "30 - x" $ do
      let initial = [P1, P1]
      it "starts with 30" $ do
        initial `shouldScore` "30 - love"
      it "a point is 15" $ do
        (initial ++ [P2]) `shouldScore` "30 - 15"
      it "two point is 30" $ do
        (initial ++ [P2, P2]) `shouldScore` "30 - 30"
      it "three point is 40" $ do
        (initial ++ [P2, P2, P2]) `shouldScore` "30 - 40"
      it "four point is game" $ do
        (initial ++ [P2, P2, P2, P2]) `shouldScore` "Game P2"
    describe "x - 40" $ do
      let initial = [P2, P2, P2]
      it "starts with 40" $ do
        initial `shouldScore` "love - 40"
      it "a point is 15" $ do
        (initial ++ [P1]) `shouldScore` "15 - 40"
      it "two point is 30" $ do
        (initial ++ [P1, P1]) `shouldScore` "30 - 40"
    describe "40 - x" $ do
      let initial = [P1, P1, P1]
      it "starts with 30" $ do
        initial `shouldScore` "40 - love"
      it "a point is 15" $ do
        (initial ++ [P2]) `shouldScore` "40 - 15"
      it "two point is 30" $ do
        (initial ++ [P2, P2]) `shouldScore` "40 - 30"
    describe "deuce" $ do
      let initial = [P1, P1, P1, P2, P2, P2]
      it "starts in deuce" $ do
        initial `shouldScore` "Deuce"
      describe "p1 win" $ do
        it "advantage" $ do
          (initial ++ [P1]) `shouldScore` "Advantage P1"
        it "win" $ do
          (initial ++ [P1, P1]) `shouldScore` "Game P1"
      describe "p2 win" $ do
        it "advantage" $ do
          (initial ++ [P2]) `shouldScore` "Advantage P2"
        it "win" $ do
          (initial ++ [P2, P2]) `shouldScore` "Game P2"
      it "loop" $ do
        (initial ++ [P2, P1]) `shouldScore` "Deuce"