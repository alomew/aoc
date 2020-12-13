{-# LANGUAGE TupleSections #-}

module Day13 where

import AOC
import Control.Monad (foldM)
import Data.Either
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

toy = "939\n7,13,x,x,59,x,31,19"

times :: Parser [Int]
times = mapMaybe (read <$>) <$> ((many1 digit <&> Just) <|> (char 'x' $> Nothing)) `sepBy` char ','

parser1 :: String -> (Int, [Int])
parser1 str = (read f, fromRight [] $ parse times "" s)
  where
    [f, s] = lines str

earliest :: Int -> Int -> Int
earliest now interval
  | now `rem` interval == 0 = 0
  | otherwise =
    (interval * (now `quot` interval + 1))
      - now

part1 :: (Int, [Int]) -> Int
part1 (goal, ivals) = id * earliest goal id
  where
    id = minimumBy (compare `on` earliest goal) ivals

indexedIds :: Parser [(Integer, Integer)]
indexedIds =
  catMaybes
    . imap (\i m -> (fromIntegral i,) <$> m)
    <$> choice
      [ many1 digit <&> Just . read,
        char 'x' $> Nothing
      ]
    `sepBy` char ','

parser2 :: String -> [(Integer, Integer)]
parser2 s = fromRight [] $ parse indexedIds "" l
  where
    [_, l] = lines s

chinese :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
chinese (r1, n1) (r2, n2) = case eea n1 n2 of
  (1, m1, m2) -> Just (r1 * m2 * n2 + r2 * m1 * n1, n1 * n2)
  _ -> Nothing

chineseRemainder :: [(Integer, Integer)] -> Maybe (Integer, Integer)
chineseRemainder [] = Nothing
chineseRemainder (x : xs) = foldM chinese x xs

part2 :: [(Integer, Integer)] -> Integer
part2 xs = x `cong` n
  where
    (x, n) = fromMaybe (0, 1) (chineseRemainder (xs <&> _1 %~ negate))

main = do
  real <- readFile "day13.input"

  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 (parser1 toy) `shouldBe` 295
      it "real" $ part1 (parser1 real) `shouldBe` 203

    describe "part2" $ do
      it "toy" $ part2 (parser2 toy) `shouldBe` 1068781
      it "real" $ part2 (parser2 real) `shouldBe` 905694340256752