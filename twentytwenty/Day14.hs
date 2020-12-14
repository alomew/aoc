module Day14 where

import AOC
import Data.Either (fromRight)
import Data.List
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String

-- DO NOT TRY PART 2 ON THIS DATA
toy = parser "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"

toy2 = parser "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"

singleMask :: Parser [(Int, String, Int)]
singleMask = do
  try $ string "mask = "
  mask <- many1 (char 'X' <|> digit)
  newline
  ps <-
    sepEndBy
      ( do
          try $ string "mem["
          address <- read <$> many1 digit
          string "] = "
          value <- read <$> many1 digit
          pure (address, value)
      )
      newline
  pure $ map (\(a, v) -> (a, mask, v)) ps

program :: Parser [(Int, String, Int)]
program = concat <$> singleMask `sepBy` (lookAhead . try $ string "mask")

parser :: String -> [(Int, String, Int)]
parser = fromRight [] . parse program ""

applyMask1 :: String -> Int -> Int
applyMask1 mask n =
  zipWith (\m d -> if m == 'X' then d else m) (reverse mask) nBinary
    & reverse
    & fromBinary
  where
    nBinary = reverse (toBinary n) ++ zeros

zeros = ['0', '0' ..]

part1 :: [(Int, String, Int)] -> Int
part1 = sum . M.elems . M.map (uncurry applyMask1) . M.fromList . map (\(x, y, z) -> (x, (y, z)))

applyMask2 :: Int -> String -> [Int]
applyMask2 adrs mask =
  map (fromBinary . reverse) . floating $ zipWith ovwrt (reverse mask) adrsBin
  where
    adrsBin = reverse (toBinary adrs) ++ zeros
    ovwrt m d
      | m == '0' = d
      | otherwise = m

floating :: String -> [String]
floating s =
  case elemIndex 'X' s of
    Nothing -> pure s
    Just i -> do
      end <- floating $ drop (i + 1) s
      k <- ['0', '1']
      pure $ take i s ++ [k] ++ end

part2 :: [(Int, String, Int)] -> Int
part2 l = sum . M.elems . M.fromList $ do
  (adrs', mask, val) <- l
  adrs <- applyMask2 adrs' mask
  pure (adrs, val)

main = do
  real <- parser <$> readFile "day14.input"

  hspec $ do
    describe "part 1" $ do
      it "toy" $ part1 toy `shouldBe` 165
      it "real" $ part1 real `shouldBe` 4886706177792

    describe "applyMask2" $ do
      it "example 1" $ applyMask2 42 "X1001X" `shouldMatchList` [26, 27, 58, 59]

    describe "part 2" $ do
      it "toy" $ part2 toy2 `shouldBe` 208
      it "real" $ part2 real `shouldBe` 3348493585827