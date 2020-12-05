module Day3 where

import AOC

data Terrain = Terrain [String] Int Int

toyData :: String
toyData = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

terrain :: String -> Terrain
terrain s = Terrain (lines s) (length . head $ ls) (length ls)
  where
    ls = lines s

-- | 'coords width height dx dy' is the positions of a trajectory
coords :: Int -> Int -> Int -> Int -> [(Int, Int)]
coords w h dx dy =
  takeWhile ((< h) . snd) [(dx * k `mod` w, dy * k) | k <- [0 ..]]

trees :: Terrain -> Int -> Int -> Int
trees (Terrain m w h) dx dy = length . filter (\(x, y) -> m !! y !! x == '#') $ coords w h dx dy

part1 :: Terrain -> Int
part1 t = trees t 3 1

part2 :: Terrain -> Int
part2 t = product $ map (uncurry $ trees t) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main =
  mainAOC 3 terrain $
    def
      & part1F ?~ part1
      & part2F ?~ part2
      & part1Ans ?~ 205
      & part2Ans ?~ 3952146825