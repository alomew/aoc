module Day6 where

import AOC
import Data.List.Split (splitOn)
import qualified Data.Set as S

toy = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

anyYes :: String -> S.Set Char
anyYes = foldl1 S.union . map S.fromList . lines

part1 :: String -> Int
part1 = sum . map (S.size . anyYes) . splitOn "\n\n"

allYes :: String -> S.Set Char
allYes = foldl1 S.intersection . map S.fromList . lines

part2 :: String -> Int
part2 = sum . map (S.size . allYes) . splitOn "\n\n"

main = do
  real <- readFile "day6.input"
  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 toy `shouldBe` 11
      it "real" $ part1 real `shouldBe` 6911
    describe "allYes" $ do
      it "group 1" $ allYes "abc" `shouldBe` S.fromList ['a', 'b', 'c']
      it "group 2" $ allYes "a\nb\nc" `shouldBe` S.empty
    describe "part2" $ do
      it "toy" $ part2 toy `shouldBe` 6
      it "real" $ part2 real `shouldBe` 3473