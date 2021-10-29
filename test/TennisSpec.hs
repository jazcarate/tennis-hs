module TennisSpec
  ( spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

------------------
---   CODE     ---
------------------

data Game = Game
  { player1 :: Int
  , player2 :: Int
  }
data Point = P1 | P2

point :: Game -> Point -> Game
point game P1 = game { player1 = 1 + (player1 game) }
point game P2 = Game (player1 game) (succ (player2 game))


mkGame :: Game
mkGame = Game 0 0

score :: Game -> String
score x = toString (player1 x) <> " - " <> toString (player2 x)

toString 0 = "love"
toString 1 = "15"
toString 2 = "30"
toString 3 = "40"







------------------
---   TEST     ---
------------------

shouldScore :: [Point] -> String -> Expectation
shouldScore ps expected = (score $ foldl point mkGame ps) `shouldBe` expected

spec :: Spec
spec = do
  describe "scoring" $ do
    focus $ describe "x - love" $ do
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
