module Day15 where

import AOC
import Control.Monad
import Control.Monad.ST
import Data.List.Split
import qualified Data.Vector.Mutable as V

foldM' b as f = foldM f b as

part :: Int -> [Int] -> Int
part n starters
  | n < length starters = starters !! (n - 1)
  | otherwise = runST $ do
    tab <- V.replicate (maximum (n : starters) + 1) (-1)
    forM_ (zip [1 ..] (init starters)) $ \(i, starter) -> V.write tab starter i
    foldM' (last starters) [length starters .. n - 1] $ \num turn -> do
      lastTurn <- (non (-1) #) <$> V.read tab num
      let diff = maybe 0 (turn -) lastTurn
      V.write tab num turn
      pure diff

real = map read $ splitOn "," "12,1,16,3,11,0"

toy = map read $ splitOn "," "0,3,6"

main =
  hspec $ do
    describe "part1" $ do
      it "toy1" $ part 4 toy `shouldBe` 0
      it "toy2" $ part 9 toy `shouldBe` 4
      it "toy3" $ part 2020 toy `shouldBe` 436
      it "real" $ part 2020 real `shouldBe` 1696

    describe "part2" $ do
      it "toy" $ part 30000000 toy `shouldBe` 175594
      it "real" $ part 30000000 real `shouldBe` 37385