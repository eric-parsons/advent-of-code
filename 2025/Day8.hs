-- See https://adventofcode.com/2025/day/8.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

import Data.List
import Data.List.Extra (sortOn, splitOn)
import Data.Maybe

-- A triplet of [x,y,z] coordinates.
type Junction = [Int]

type Connection = (Junction, Junction)

type Circuit = [Junction]

main = do
  input <- readFile "./Day8.input.txt"
  let junctions = parseInput input
  print $ part1 junctions
  print $ part2 junctions

part1 :: [Junction] -> Int
part1 js = product $ take 3 $ sortDesc $ map length $ connectAll (take 1000 $ connections js) initCircuits
  where
    initCircuits = map (: []) js
    connectAll [] cts = cts
    connectAll (cn : cns) cts = connectAll cns $ connect cn cts

part2 :: [Junction] -> Int
part2 js = finalConnection (connections js) initCircuits
  where
    initCircuits = map (: []) js
    finalConnection (cn@(p, q) : cns) cts = case connect cn cts of
      [_] -> head p * head q
      cts' -> finalConnection cns cts'

connect :: Connection -> [Circuit] -> [Circuit]
connect (p, q) cts
  | ct1 == ct2 = cts
  | otherwise = (ct1 ++ ct2) : delete ct1 (delete ct2 cts)
  where
    ct1 = fromJust $ find (p `elem`) cts
    ct2 = fromJust $ find (q `elem`) cts

-- Produces unique pairs of junctions sorted by distance (closest first).
connections :: [Junction] -> [Connection]
connections = sortOn distance . mkPairs
  where
    mkPairs [] = []
    mkPairs (p : ps) = [(p, q) | q <- ps] ++ mkPairs ps

distance :: Connection -> Double
distance (p, q) = sqrt $ fromIntegral $ sum $ map (^ 2) $ zipWith (-) p q

sortDesc = sortBy (flip compare)

parseInput :: String -> [Junction]
parseInput = map (map read . splitOn ",") . lines
