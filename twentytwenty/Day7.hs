module Day7 where

import AOC
import Data.Graph
import qualified Data.Map as M
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

toy = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"

inBag :: Parser (Int, String)
inBag =
  do
    d <- P.many1 P.digit
    P.space
    adj <- P.many1 P.letter
    P.space
    col <- P.many1 P.letter
    P.space
    wordBag
    pure (read d, adj <> " " <> col)

wordBag :: Parser ()
wordBag = P.string "bag" >> P.optional (P.char 's')

oneBag :: Parser (String, [(Int, String)])
oneBag = do
  desc <- P.many1 P.letter
  P.space
  col <- P.many1 P.letter
  P.space
  wordBag
  P.space
  P.string "contain"
  P.space
  contained <-
    (inBag `P.sepBy1` P.string ", ")
      <|> (P.string "no other bags" >> pure [])

  P.char '.'
  pure (desc <> " " <> col, contained)

type Rule = (String, [(Int, String)])

bagGraph :: Parser [Rule]
bagGraph = oneBag `P.endBy` P.newline

part1 :: [Rule] -> String -> Maybe Int
part1 as desired = dvtx <&> \vtx -> (+ (-1)) $ count (\x -> path graph x vtx) $ vertices graph
  where
    (graph, _, vertexFromKey) = graphFromEdges $ map (\(s, ns) -> ("", s, map snd ns)) as
    dvtx = vertexFromKey desired

part2 :: [Rule] -> String -> Int
part2 rs = go
  where
    m = M.fromList rs
    go s = sum . map (\(i, s') -> i * (1 + go s')) $ m M.! s

main = do
  real <- readFile "day7.input"
  let Right realRs = P.parse bagGraph "" real
  let Right toyRs = P.parse bagGraph "" toy
  hspec $ do
    describe "part1" $ do
      it "toy" $ part1 toyRs "shiny gold" `shouldBe` Just 4
      it "real" $ part1 realRs "shiny gold" `shouldBe` Just 119
    describe "part2" $ do
      it "toy" $ part2 toyRs "shiny gold" `shouldBe` 32
      it "real" $ part2 realRs "shiny gold" `shouldBe` 155802