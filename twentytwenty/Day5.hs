module Day5 where

import AOC
import qualified Data.Set as S
import Test.Hspec
import Text.Printf

binString ::
  -- | zeroChar
  Char ->
  -- | oneChar
  Char ->
  -- | encoded binary number
  String ->
  Int
binString z o = decomp . reverse
  where
    decomp :: String -> Int
    decomp = ifoldl (\i n c -> 2 ^ i * f c + n) 0
    f c
      | c == z = 0
      | c == o = 1
      | otherwise = error (printf "looking for %c and %c: found %c" z o c)

parseRow = binString 'F' 'B'

parseCol = binString 'L' 'R'

parseSeat :: String -> (Int, Int)
parseSeat s = (parseRow r, parseCol c)
  where
    (r, c) = splitAt 7 s

seatId :: String -> Int
seatId s = r * 8 + c
  where
    (r, c) = parseSeat s

part1 :: [String] -> Int
part1 = maximum . map seatId

part2 :: [String] -> Int
part2 xs = head . dropWhile (`S.member` occupied) . dropWhile (`S.notMember` occupied) $ [0 ..]
  where
    occupied :: S.Set Int
    occupied = S.fromList $ map seatId xs

main = do
  realData <- lines <$> readFile "day5.input"
  hspec $ do
    describe "binString" $ do
      it "row example" $ do
        parseRow "FBFBBFF" `shouldBe` 44
      it "col example" $ do
        parseCol "RLR" `shouldBe` 5
    describe "full seat" $ do
      it "example" $ do
        parseSeat "FBFBBFFRLR" `shouldBe` (44, 5)

    describe "part1" $ do
      it "example 1" $ part1 ["BFFFBBFRRR"] `shouldBe` 567
      it "example 2" $ part1 ["FFFBBBFRRR"] `shouldBe` 119
      it "example 3" $ part1 ["BBFFBBFRLL"] `shouldBe` 820
      it "real" $ part1 realData `shouldBe` 883

    describe "part2" $ do
      it "real" $ part2 realData `shouldBe` 532