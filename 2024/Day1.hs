import Data.List
import Data.Maybe

-- See https://adventofcode.com/2024/day/1.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

main = do
  input <- readFile "./Day1.input.txt"
  let (ls, rs) = parseInput input
  print $ part1 ls rs
  print $ part2 ls rs

part1 ls = sum . map abs . zipWith (-) (sort ls) . sort

part2 ls rs = sum $ map (\x -> x * freq x) ls
  where
    freq = fromMaybe 0 . flip lookup table
    table = map (\g -> (head g, length g)) $ group $ sort rs

-- Produces two lists of numbers from the two columns in the input.
parseInput = unzip . map toPair . lines
  where
    -- Parse e.g. "123 456" into (123, 456).
    toPair s = let [x, y] = map read $ words s in (x, y)
