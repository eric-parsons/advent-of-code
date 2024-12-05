import Data.List

-- See https://adventofcode.com/2024/day/2.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

main = do
  input <- readFile "./Day2.input.txt"
  let reports = map (map read . words) $ lines input
  print $ part1 reports
  print $ part2 reports

part1 = checkWith isSafe

part2 = checkWith isSafeWithDampener

checkWith f = sum . map (fromEnum . f)

isSafe report = all (\d -> abs d > 0 && abs d <= 3 && signum d == direction) deltas
  where
    -- The deltas between adjacent levels in the report.
    deltas = map (uncurry (-)) $ siblings report
    direction = signum (head deltas)

isSafeWithDampener report = isSafe report || any isSafe subReports
  where
    subReports = [s | s <- subsequences report, length s == length report - 1]

-- Produces all pairs of adjacent items in a list.
siblings (x1 : x2 : xs) = (x1, x2) : siblings (x2 : xs)
siblings _ = []
