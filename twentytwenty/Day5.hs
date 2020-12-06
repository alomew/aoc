module Day5 where

import AOC
import qualified Data.Set as S
import Text.Printf

binString ::
  -- | zeroChars
  [Char] ->
  -- | oneChars
  [Char] ->
  -- | encoded binary number
  String ->
  Int
binString zs os = decomp . reverse
  where
    decomp :: String -> Int
    decomp = ifoldl (\i n c -> 2 ^ i * f c + n) 0
    f c
      | c `elem` zs = 0
      | c `elem` os = 1
      | otherwise = error (printf "valid chars are %v: found %c" (zs ++ os) c)

seatId :: String -> Int
seatId = binString ['F', 'L'] ['B', 'R']

minMax :: [Int] -> (Int, Int)
minMax xs = (minimum, maximum) & both %~ ($ xs)

part1 :: [String] -> Int
part1 = snd . minMax . map seatId

part2 :: [String] -> Int
part2 xs = total - sum ids
  where
    ids = map seatId xs
    (mn, mx) = minMax $ map seatId xs
    total = (mx - mn + 1) * ((mn + mx) `quot` 2)

main = do
  realData <- lines <$> readFile "day5.input"
  hspec $ do
    describe "binString" $ do
      it "row example" $ do
        seatId "FBFBBFF" `shouldBe` 44
      it "col example" $ do
        seatId "RLR" `shouldBe` 5
    describe "full seat" $ do
      it "example" $ do
        seatId "FBFBBFFRLR" `shouldBe` 44 * 8 + 5

    describe "part1" $ do
      it "example 1" $ part1 ["BFFFBBFRRR"] `shouldBe` 567
      it "example 2" $ part1 ["FFFBBBFRRR"] `shouldBe` 119
      it "example 3" $ part1 ["BBFFBBFRLL"] `shouldBe` 820
      it "real" $ part1 realData `shouldBe` 883

    describe "part2" $ do
      it "real" $ part2 realData `shouldBe` 532