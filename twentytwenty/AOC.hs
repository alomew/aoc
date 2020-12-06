{-# LANGUAGE TemplateHaskell #-}

module AOC (module Control.Lens, module Data.Default, module Test.Hspec, module AOC) where

import Control.Lens
import Data.Default
import Test.Hspec
import Text.Printf

-- TIP: ?~ for setting the maybe values
data AOCOpts a = AOCOpts
  { _part1F :: Maybe (a -> Int),
    _part2F :: Maybe (a -> Int),
    _part1Ans :: Maybe Int,
    _part2Ans :: Maybe Int
  }

makeLenses ''AOCOpts

instance Default (AOCOpts a) where
  def = AOCOpts {_part1F = Nothing, _part2F = Nothing, _part1Ans = Nothing, _part2Ans = Nothing}

mainAOC :: Int -> (String -> a) -> AOCOpts a -> IO ()
mainAOC day parser opts = do
  input <- parser <$> readFile (printf "day%d.input" day)
  let formatAns part =
        "Part " <> show part <> ": "
          <> let acc = (opts ^. partF) ?? input
                 expect = opts ^. partAns
                 (partF, partAns) = case part of
                   1 -> (part1F, part1Ans)
                   2 -> (part2F, part2Ans)
              in ( case acc of
                     Just acc' -> show acc' <> rightwrong
                       where
                         rightwrong = maybe "" (\e -> " " <> if e == acc' then "✓" else "✗") expect
                     Nothing -> ""
                 )
  putStrLn $ formatAns 1
  putStrLn $ formatAns 2

-- helper methods

count :: (a -> Bool) -> [a] -> Int
count f = foldl (\c x -> if f x then c + 1 else c) 0

rightToMaybe :: Either b a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe (Left _) = Nothing