{-# LANGUAGE TemplateHaskell #-}

module AOC (module AOC, module Control.Lens, module Data.Default) where

import Control.Lens
import Data.Default
import Data.Maybe (fromMaybe)
import Text.Printf

data Toy = Toy String (Maybe Int) (Maybe Int) deriving (Show)

-- TIP: ?~ for setting the maybe values
data AOCOpts a = AOCOpts
  { _toys :: [Toy],
    _part1F ::
      a ->
      Int,
    _part2F :: a -> Int,
    _part1Ans :: Maybe Int,
    _part2Ans :: Maybe Int
  }

makeLenses ''AOCOpts

instance Default (AOCOpts a) where
  def = AOCOpts {_toys = [], _part1F = const 2020, _part2F = const 2020, _part1Ans = Nothing, _part2Ans = Nothing}

runToy' :: (String -> a) -> (a -> Int) -> (a -> Int) -> Toy -> (Maybe Bool, Maybe Bool)
runToy' pars p1 p2 (Toy s a1 a2) = (go p1 a1, go p2 a2)
  where
    go p a = (== (p $ pars s)) <$> a

runToy :: (String -> a) -> (a -> Int) -> (a -> Int) -> Toy -> Bool
runToy pars p1 p2 t = f z1 && f z2
  where
    (z1, z2) = runToy' pars p1 p2 t
    f = fromMaybe True

mainAOC :: Int -> (String -> a) -> AOCOpts a -> IO ()
mainAOC day parser opts = do
  if all (runToy parser (opts ^. part1F) (opts ^. part2F)) $ opts ^. toys
    then do
      input <- parser <$> readFile (printf "day%d.input" day)
      let formatAns part = "Part " <> show part <> ": " <> show acc <> rightwrong
            where
              rightwrong = maybe "" (\e -> " " <> if e == acc then "✓" else "✗") expect
              acc = (opts ^. partF) input
              expect = opts ^. partAns
              (partF, partAns) = case part of
                1 -> (part1F, part1Ans)
                2 -> (part2F, part2Ans)

      putStrLn $ formatAns 1
      putStrLn $ formatAns 2
    else print $ zip ts (map (runToy' parser (opts ^. part1F) (opts ^. part2F)) ts)
  where
    ts = opts ^. toys