{-# LANGUAGE TemplateHaskell #-}

module Day12 where

import AOC
import Data.Either (rights)
import Text.Parsec
import Text.Parsec.String

data Ship = Ship {_bearing :: Int, _x :: Int, _y :: Int}
  deriving (Show)

makeLenses ''Ship

toy = mvmnts "F10\nN3\nF7\nR90\nF11"

north = (0, 1)

east = (1, 0)

south = (0, -1)

west = (-1, 0)

bearingXY 0 = north
bearingXY 1 = east
bearingXY 2 = south
bearingXY 3 = west
bearingXY x = bearingXY $ x `cong` 4

data Move = Twist Int | Slide (Int, Int) | Forward Int

moveShip :: Ship -> Move -> Ship
moveShip s (Twist p) = s & bearing %~ ((`mod` 4) . (+ p))
moveShip s (Slide (dx, dy)) =
  s & x +~ dx
    & y +~ dy
moveShip s (Forward p) =
  s & x +~ p * dx
    & y +~ p * dy
  where
    (dx, dy) = s ^. bearing & bearingXY

slide :: (Int, Int) -> Parser Move
slide dd = do
  n <- read <$> many1 digit
  pure $ Slide (dd & both *~ n)

mvmnt :: Parser Move
mvmnt =
  choice
    [ char 'N' >> slide north,
      char 'S' >> slide south,
      char 'E' >> slide east,
      char 'W' >> slide west,
      (char 'L' >> ((`div` 90) . negate . read <$> many1 digit)) <&> Twist,
      (char 'R' >> ((`div` 90) . read <$> many1 digit)) <&> Twist,
      char 'F' >> (read <$> many1 digit) <&> Forward
    ]

mvmnts :: String -> [Move]
mvmnts = rights . map (parse mvmnt "") . lines

part1 :: [Move] -> Int
part1 mvs =
  foldl moveShip (Ship 1 0 0) mvs
    & \s -> abs (s ^. x) + abs (s ^. y)

data ShipW = ShipW Ship (Int, Int)

rotR90 :: (Int, Int) -> (Int, Int)
rotR90 (x, y) = (y, - x)

moveShipW :: ShipW -> Move -> ShipW
moveShipW (ShipW s (xw, yw)) (Slide (dx, dy)) = ShipW s (xw + dx, yw + dy)
moveShipW (ShipW s wco) (Twist n) = ShipW s newco
  where
    newco = applyN (n `cong` 4) rotR90 wco
moveShipW (ShipW s wco) (Forward n) =
  ShipW
    ( s & x +~ dx
        & y +~ dy
    )
    wco
  where
    (dx, dy) = wco & both *~ n

part2 :: [Move] -> Int
part2 mvs =
  foldl moveShipW (ShipW (Ship 1 0 0) (10, 1)) mvs
    & \(ShipW s _) -> abs (s ^. x) + abs (s ^. y)

main = do
  real <- mvmnts <$> readFile "day12.input"

  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 toy `shouldBe` 25
      it "real" $ part1 real `shouldBe` 938

    describe "part2" $ do
      it "toy" $ part2 toy `shouldBe` 286
      it "real" $ part2 real `shouldBe` 54404