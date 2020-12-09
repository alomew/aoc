module Day9 where

import AOC
import Data.List (tails)
import Data.List.Split (divvy)
import Data.Maybe (mapMaybe)

toyS = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n"

-- | the pairs of distinct numbers in 'ns' summing to 'goal'
summingPairs :: [Int] -> Int -> [(Int, Int)]
summingPairs ns goal =
  [(x, y) | x : xs <- tails ns, x <= goal, y : _ <- tails xs, x /= y, x + y == goal]

parse :: String -> [Int]
parse = map read . lines

badTrain :: [Int] -> Maybe Int
badTrain ns
  | not . null $ summingPairs (init ns) (last ns) =
    Nothing
  | otherwise = Just $ last ns

part1 :: Int -> [Int] -> Int
part1 n = head . mapMaybe badTrain . divvy (n + 1) 1

contiguousSum :: [Int] -> Int -> Maybe [Int]
contiguousSum xs goal = go [] xs
  where
    go ms _ | sum ms == goal = Just ms
    go ms@(_ : mss) ns
      | sum ms > goal = go mss ns
    go _ [] = Nothing
    -- in this last branch, deduce 'sum ms < goal'
    go ms (n : ns) = go (ms `snoc` n) ns

part2 :: Int -> [Int] -> Maybe Int
part2 n xs = res <$> contiguousSum xs p1
  where
    res ns = minimum ns + maximum ns
    p1 = part1 n xs

main = do
  real <- parse <$> readFile "day9.input"
  let toy = parse toyS

  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 5 toy `shouldBe` 127
      it "real" $ part1 25 real `shouldBe` 138879426
    describe "part2" $ do
      it "toy" $ part2 5 toy `shouldBe` Just 62
      it "real" $ part2 25 real `shouldBe` Just 23761694
