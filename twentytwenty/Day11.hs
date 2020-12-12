module Day11 where

import AOC hiding (Empty)
import Data.Foldable (find)
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)

toy = layout "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"

data Seat = Occupied | Empty | Floor deriving (Show, Eq)

type Layout = Map (Int, Int) Seat

rowOccupiedIxs :: String -> [Int]
rowOccupiedIxs = catMaybes . imap (\i c -> if c == 'L' then Just i else Nothing)

layout :: String -> Layout
layout = Map.fromList . concat . imap (\i r -> [((i, j), Occupied) | j <- rowOccupiedIxs r]) . lines

deltas :: [(Int, Int)]
deltas =
  delete
    (0, 0)
    [ (i, j) | i <- [-1, 0, 1], j <- [-1, 0, 1]
    ]

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (i, j) =
  [(i + k, j + l) | (k, l) <- deltas]

allAdjacents :: Layout -> Map (Int, Int) [(Int, Int)]
allAdjacents =
  Map.mapWithKey f
  where
    f (i, j) _ = adjacents (i, j)

phaseSeat ::
  Int ->
  Seat ->
  [Seat] ->
  Seat
phaseSeat i s oss
  | s == Empty && count (== Occupied) oss == 0 = Occupied
  | s == Occupied && count (== Occupied) oss >= i = Empty
  | otherwise = s

phase1 :: Map (Int, Int) [(Int, Int)] -> Layout -> Layout
phase1 adjs l =
  Map.mapWithKey
    (\co s -> phaseSeat 4 s (mapMaybe (l Map.!?) $ adjs Map.! co))
    l

part1 :: Layout -> Int
part1 l = count (== Occupied) . Map.elems . fixPoint (phase1 $ allAdjacents l) $ l

phase2 :: Map (Int, Int) [(Int, Int)] -> Layout -> Layout
phase2 vs l =
  Map.mapWithKey
    (\co s -> phaseSeat 5 s (mapMaybe (l Map.!?) $ fromMaybe [] (vs Map.!? co)))
    l

sightlines :: Layout -> Map (Int, Int) [(Int, Int)]
sightlines l =
  Map.mapWithKey f l
  where
    f (i, j) _ = mapMaybe inDir deltas
      where
        inDir (di, dj) =
          [(i + n * di, j + n * dj) | n <- [1 ..]]
            & takeWhile (\(x, y) -> x >= 0 && y >= 0 && x <= w && y <= h)
            & find (\co -> isJust $ l Map.!? co)
    w = maximum . map fst $ Map.keys l
    h = maximum . map snd $ Map.keys l

part2 :: Layout -> Int
part2 l = count (== Occupied) . Map.elems $ fixPoint (phase2 (sightlines l)) l

main = do
  real <- layout <$> readFile "day11.input"

  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 toy `shouldBe` 37
      it "real" $ part1 real `shouldBe` 2494

    describe "part2" $ do
      it "toy" $ part2 toy `shouldBe` 26
      it "real" $ part2 real `shouldBe` 2306