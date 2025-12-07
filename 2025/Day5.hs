-- See https://adventofcode.com/2025/day/5.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

import Data.List.Extra -- Requires package "extra".

type Id = Int

type Range = (Id, Id)

main = do
  input <- readFile "./Day5.input.txt"
  let (ranges, ids) = parseInput input
      ranges' = mergeRanges ranges
  print $ part1 ranges' ids
  print $ part2 ranges'

part1 :: [Range] -> [Id] -> Int
part1 ranges ids = length $ filter (\id -> any (\(lower, upper) -> id >= lower && id <= upper) ranges) ids

part2 :: [Range] -> Int
part2 = sum . map (\(lower, upper) -> upper - lower + 1)

-- Optimize ranges so that there is no overlap between any two.
mergeRanges :: [Range] -> [Range]
mergeRanges = mergeAdjacent . sortOn fst
  where
    mergeAdjacent (a@(lower1, upper1) : b@(lower2, upper2) : rest)
      | upper1 >= lower2 || lower2 - upper1 == 1 = mergeAdjacent ((lower1, max upper1 upper2) : rest)
      | otherwise = a : mergeAdjacent (b : rest)
    mergeAdjacent xs = xs

parseInput :: String -> ([Range], [Id])
parseInput input = (ranges, ids)
  where
    [rangesInput, idsInput] = splitOn "\n\n" input
    ranges = map toRange $ lines rangesInput
    ids = map read $ lines idsInput
    toRange str = let [lower, upper] = wordsBy (== '-') str in (read lower, read upper)
