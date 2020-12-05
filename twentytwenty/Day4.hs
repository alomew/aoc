module Day4 where

import AOC
import Data.Char
import Data.Either (rights)
import Data.Functor (($>))
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe (isJust)
import Test.Hspec
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data Length = Cm Int | In Int deriving (Show)

data EyeCol = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving (Enum, Show)

allEyeCols = [Amb .. Oth]

type Passport = M.Map Field String

data Field = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid deriving (Eq, Show, Ord, Bounded, Enum)

chari :: Char -> Parser Char
chari c = P.char (toLower c) <|> P.char (toUpper c)

stringi :: String -> Parser String
stringi = mapM chari

reqFields :: [Field]
reqFields = [Byr .. Pid]

allFields :: [Field]
allFields = [Byr .. Cid]

key :: (Show a) => a -> Parser a
key f = (P.try . stringi $ show f) >> pure f

keyValue :: Parser (Field, String)
keyValue = do
  f <- P.choice $ map key allFields
  P.char ':'
  v <- P.manyTill P.anyChar ((P.space $> ()) <|> P.eof)
  pure (f, v)

passport :: Parser Passport
passport =
  M.fromList
    <$> keyValue `P.sepBy` P.spaces

parser :: String -> [Passport]
parser = rights . map (P.parse passport "") . splitOn "\n\n"

toyData :: [Passport]
toyData = parser "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

hasReqFields :: Passport -> Bool
hasReqFields p = null $ reqFields \\ M.keys p

-- |
-- expect 2:
-- >>> part1 toyData
-- 2
part1 :: [Passport] -> Int
part1 = count hasReqFields

valRange :: Int -> Int -> Int -> Maybe Int
valRange mini maxi n = if mini <= n && n <= maxi then Just n else Nothing

valByr :: String -> Maybe Int
valByr = valRange 1920 2002 . read

valIyr :: String -> Maybe Int
valIyr = valRange 2010 2020 . read

valEyr :: String -> Maybe Int
valEyr = valRange 2020 2030 . read

lengthP :: Parser Length
lengthP = do
  ds <- read <$> P.many1 P.digit
  unit <- (P.try (P.string "cm") $> Cm) <|> (P.try (P.string "in") $> In)
  pure $ unit ds

valHgt :: String -> Maybe Length
valHgt s = do
  l <- rightToMaybe $ P.parse lengthP "" s
  case l of
    Cm n -> Cm <$> valRange 150 193 n
    In n -> In <$> valRange 59 76 n

hexColor :: Parser String
hexColor = do
  P.char '#'
  P.count 6 (P.digit <|> P.satisfy (`elem` ['a' .. 'f'])) <* P.eof

valHcl :: String -> Maybe String
valHcl = rightToMaybe . P.parse hexColor ""

valEcl :: String -> Maybe EyeCol
valEcl = rightToMaybe . P.parse (P.choice $ map key allEyeCols) ""

valPid :: String -> Maybe String
valPid = rightToMaybe . P.parse (P.count 9 P.digit <* P.eof) ""

valPassport :: Passport -> Bool
valPassport p =
  isJust $ do
    valByr =<< p M.!? Byr
    valIyr =<< p M.!? Iyr
    valEyr =<< p M.!? Eyr
    valHgt =<< p M.!? Hgt
    valHcl =<< p M.!? Hcl
    valEcl =<< p M.!? Ecl
    valPid =<< p M.!? Pid

part2 :: [Passport] -> Int
part2 = count valPassport

main = do
  realData <- parser <$> readFile "day4.input"
  hspec $ do
    describe "part1" $ do
      it "works on toy" $ do
        part1 toyData `shouldBe` 2
      it "works on real" $ do
        part1 realData `shouldBe` 206
    describe "valHgt" $ do
      it "a" $ do
        valHgt "60in" `shouldSatisfy` isJust
    describe "part2" $ do
      it "works on real" $ do
        part2 realData `shouldBe` 123