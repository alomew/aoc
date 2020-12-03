module Day2 where

import Data.Either (rights)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

stringToPEs :: String -> [PassEntry]
stringToPEs = rights . map (P.parse passEntryP "") . lines

toyData :: [PassEntry]
toyData = stringToPEs "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

realData :: IO [PassEntry]
realData = stringToPEs <$> readFile "day2.input"

data PassEntry = PassEntry {minBound :: Int, maxBound :: Int, passchar :: Char, password :: String} deriving (Show)

passEntryP :: Parser PassEntry
passEntryP = do
  minb <- P.many1 P.digit
  P.char '-'
  maxb <- P.many1 P.digit
  P.space
  pc <- P.letter
  P.string ": "
  pw <- P.many1 P.letter
  pure $ PassEntry (read minb) (read maxb) pc pw

validPasswordShed :: PassEntry -> Bool
validPasswordShed (PassEntry minb maxb pc pw) = minb <= amount && amount <= maxb
  where
    amount = length . filter (== pc) $ pw

validPasswordToboggan :: PassEntry -> Bool
validPasswordToboggan (PassEntry minb maxb pc pw) = match minb /= match maxb
  where
    match at = pw !! (at - 1) == pc

part1 :: [PassEntry] -> Int
part1 = length . filter validPasswordShed

part2 :: [PassEntry] -> Int
part2 = length . filter validPasswordToboggan

main = do
  pes <- realData
  putStr "Part 1: "
  print $ part1 pes
  putStr "Part 2: "
  print $ part2 pes