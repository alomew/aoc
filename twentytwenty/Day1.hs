module Day1 where

-- | 'expense' constructs combinations, eliding those that would sum over the goal as it goes.
expense :: [Int] -> Int -> Int -> [[Int]]
expense [] _ _ = []
expense (x : xs) 1 goal
  | x == goal = [[x]]
  | otherwise = expense xs 1 goal
expense (x : xs) n goal
  | x <= goal = mappend (expense xs n goal) ((x :) <$> expense xs (n - 1) (goal - x))
  | otherwise = expense xs n goal

toy :: [Int]
toy = [1721, 979, 366, 299, 675, 1456]

realData :: IO [Int]
realData = map read . lines <$> readFile "day1.input"

part1 ns = product <$> expense ns 2 2020

part2 ns = product <$> expense ns 3 2020

main :: IO ()
main = do
  ns <- realData
  putStr "Part 1: "
  print . head $ part1 ns
  putStr "Part 2: "
  print . head $ part2 ns
