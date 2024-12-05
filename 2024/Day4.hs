import Data.List
import Text.Regex.PCRE

-- See https://adventofcode.com/2024/day/4.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

main = do
  input <- readFile "./Day4.input.txt"
  let rows = lines input
  print $ part1 rows
  print $ part2 rows

part1 rows = sum $ map numMatches [rows, cols, diags1, diags2]
  where
    cols = transpose rows
    diags1 = diagonals rows
    diags2 = diagonals $ reverse rows
    numMatches :: [String] -> Int
    numMatches = sum . map (\s -> s =~ "XMAS" + s =~ "SAMX")

part2 rows = sum $ map (fromEnum . isXmas) xGroups
  where
    -- Enumerate all 3x3 submatrices of the input, each concatenated into a
    -- single 9-character string for easier pattern matching.
    xGroups = [concat group | ts <- triplets rows, group <- transpose $ map triplets ts]
    isXmas s = s =~ "M.M.A.S.S|M.S.A.M.S|S.M.A.S.M|S.S.A.M.M" :: Bool

-- Produces a list of values obtained when a matrix is read diagonally from top
-- left to bottom right.
diagonals = map (filter (/= '🤓')) . transpose . zipWith (++) padding
  where
    padding = map (`replicate` '🤓') [0 ..]

-- Produces all subsequences of size 3 in a list.
triplets (x1 : x2 : x3 : xs) = [x1, x2, x3] : triplets (x2 : x3 : xs)
triplets _ = []
