-- See https://adventofcode.com/2025/day/4.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

import Data.List
import Data.Matrix -- Requires package "matrix".
import Data.Maybe

-- (row, col) where (1,1) is the top left of the grid.
type Coords = (Int, Int)

data Cell = Roll | Empty deriving (Eq)

type Grid = Matrix Cell

main = do
  input <- readFile "./Day4.input.txt"
  let grid = parseInput input
  print $ part1 grid
  print $ part2 grid

part1 :: Grid -> Int
part1 grid = length $ filter (isRemovable grid) $ allCoords grid

part2 :: Grid -> Int
part2 grid = numRolls grid - numRolls (removeRolls grid)

-- Removes any rolls from the grid that are removable.
removeRolls :: Grid -> Grid
removeRolls grid = case find (isRemovable grid) positions of
  Just pos -> removeRolls (setElem Empty pos grid)
  Nothing -> grid
  where
    positions = allCoords grid

isRemovable :: Grid -> Coords -> Bool
isRemovable grid pos@(r, c) = grid ! pos == Roll && length neighbors < 4
  where
    neighbors =
      filter (== Roll) $
        mapMaybe
          (\(r', c') -> safeGet r' c' grid)
          [ (r - 1, c - 1),
            (r - 1, c),
            (r - 1, c + 1),
            (r, c - 1),
            (r, c + 1),
            (r + 1, c - 1),
            (r + 1, c),
            (r + 1, c + 1)
          ]

numRolls :: Grid -> Int
numRolls = length . filter (== Roll) . toList

allCoords :: Grid -> [Coords]
allCoords grid = [(r, c) | r <- [1 .. nrows grid], c <- [1 .. ncols grid]]

parseInput :: String -> Grid
parseInput = fromLists . map (map toCell) . lines
  where
    toCell '@' = Roll
    toCell _ = Empty
