module Day10 where

import AOC
import Data.List
import Data.List.Split
import GHC.Arr

toy1 = parse "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"

toy2 = parse "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3\n"

realIO = parse <$> readFile "day10.input"

parse :: String -> [Int]
parse = map read . lines

diffs = map (\[a, b] -> b - a) . divvy 2 1

part1 :: [Int] -> Int
part1 xs = count (== 1) distr * count (== 3) distr
  where
    distr = diffs $ sort (0 : maximum xs + 3 : xs)

part2 :: [Int] -> Int
part2 xs' = store ! 0
  where
    xs = 0 : high : xs'
    go i = sum [store ! j | j <- xs, j - i `elem` [1, 2, 3]]
    store = array (0, high) $ (high, 1) : [(i, go i) | i <- [0 .. high - 3]]
    high = maximum xs' + 3

main = do
  real <- realIO
  hspec $ do
    describe "part 1" $ do
      it "toy1" $ part1 toy1 `shouldBe` 35
      it "toy2" $ part1 toy2 `shouldBe` 220
      it "real" $ part1 real `shouldBe` 2040
    describe "part 2" $ do
      it "toy 1" $ part2 toy1 `shouldBe` 8
      it "toy 2" $ part2 toy2 `shouldBe` 19208
      it "real" $ part2 real `shouldBe` 28346956187648
