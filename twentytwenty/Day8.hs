{-# LANGUAGE TemplateHaskell #-}

module Day8 where

import AOC
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec hiding (State)
import Text.Parsec.String

toy = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"

data Instr = Nop Int | Acc Int | Jmp Int deriving (Show)

data State = State {_ptr :: Int, _acc :: Int, _visited :: Set Int} deriving (Show)

makeLenses ''State

instr :: Parser Instr
instr = do
  kind <-
    (try (string "jmp") $> Jmp)
      <|> (try (string "acc") $> Acc)
      <|> (try (string "nop") $> Nop)
  param <- do
    space
    sgn <- (string "+" $> "") <|> string "-"
    digs <- many1 digit
    pure $ read (sgn <> digs)
  pure $ kind param

program :: Parser [Instr]
program = instr `endBy` newline

run :: [Instr] -> [State]
run is = go (State 0 0 S.empty)
  where
    go s0 = iterate step s0
      where
        step s =
          ( case inst of
              Nop _ -> s & ptr +~ 1
              Acc n -> s & acc +~ n & ptr +~ 1
              Jmp n -> s & ptr +~ n
          )
            & visited
            %~ S.insert p
          where
            p = s ^. ptr
            inst = is !! p

part1 :: [Instr] -> Int
part1 = (^. acc) . head . dropWhile (\(State ptr _ vs) -> ptr `S.notMember` vs) . run

swaps :: [Instr] -> [[Instr]]
swaps is = mapMaybe swapProg [0 .. length is - 1]
  where
    swapProg i =
      case is !! i of
        Nop n -> Just $ is & ix i .~ Jmp n
        Jmp n -> Just $ is & ix i .~ Nop n
        _ -> Nothing

terminatesWith :: Int -> [State] -> Maybe Int
terminatesWith _ [] = Nothing
terminatesWith n ((State ptr acc vs) : xs)
  | ptr `S.member` vs = Nothing
  | ptr >= n = Just acc
  | otherwise = terminatesWith n xs

part2 :: [Instr] -> Int
part2 is = head $ mapMaybe (terminatesWith $ length is) statess
  where
    statess = map run $ swaps is

main = do
  let Right toyIs = parse program "" toy
  Right realIs <- parse program "" <$> readFile "day8.input"

  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 toyIs `shouldBe` 5
      it "real" $ part1 realIs `shouldBe` 1337
    describe "part2" $ do
      it "toy" $ part2 toyIs `shouldBe` 8
      it "real" $ part2 realIs `shouldBe` 1358