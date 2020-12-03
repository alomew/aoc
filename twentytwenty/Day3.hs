module Day3 where

data Terrain = Terrain [String] Int Int

toyData :: Terrain
toyData = terrain "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

realData :: IO Terrain
realData = terrain <$> readFile "day3.input"

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
main = do
  ns <- realData
  putStr "Part 1: "
  print $ part1 ns
  putStr "Part 2: "
  print $ part2 ns