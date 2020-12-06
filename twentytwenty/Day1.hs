module Day1 where

import AOC
import Data.List (tails)

-- | 'expense' constructs combinations, eliding those that would sum over the goal as it goes.
expense :: [Int] -> Int -> Int -> [[Int]]
expense [] _ _ = []
expense (x : xs) 1 goal
  | x == goal = [[x]]
  | otherwise = expense xs 1 goal
expense (x : xs) n goal
  | x <= goal = mappend (expense xs n goal) ((x :) <$> expense xs (n - 1) (goal - x))
  | otherwise = expense xs n goal

expense2 :: [Int] -> Int -> [Int]
expense2 ps goal =
  [x * y | x : xs <- tails ps, x <= goal, y : _ <- tails xs, x + y == goal]

expense3 :: [Int] -> Int -> [Int]
expense3 ps goal =
  [x * y * z | x : xs <- tails ps, x <= goal, y : ys <- tails xs, x + y <= goal, z : _ <- tails ys, x + y + z == goal]

toy :: [Int]
toy = [1721, 979, 366, 299, 675, 1456]

realData :: IO [Int]
realData = map read . lines <$> readFile "day1.input"

part1 ns = expense2 ns 2020

part2 ns = expense3 ns 2020

main :: IO ()
main = do
  real <- realData
  hspec $ do
    describe "part1" $ do
      it "real" $
        part1 real `shouldBe` [1010299]
    describe "part2" $ do
      it "real" $
        part2 real `shouldBe` [42140160]
