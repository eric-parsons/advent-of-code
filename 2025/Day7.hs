-- See https://adventofcode.com/2025/day/7.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

import Data.List
import Data.Matrix

data Cell = Start | Splitter | Beam | Empty deriving (Eq)

type Grid = Matrix Cell

type Coords = (Int, Int)

main = do
  input <- readFile "./Day7.input.txt"
  let grid = fireBeam $ parseInput input
  print $ part1 grid
  print $ part2 grid

part1 :: Grid -> Int
part1 = countSplits

part2 :: Grid -> Int
part2 grid = countPaths grid startPos
  where
    startPos = head [(1, c) | c <- [1 .. ncols grid], grid ! (1, c) == Start]

countSplits :: Grid -> Int
countSplits grid = sum $ toList $ mapPos (\(r, c) cell -> if cell == Splitter && grid ! (r - 1, c) == Beam then 1 else 0) grid

countPaths :: Grid -> Coords -> Int
countPaths grid pos = memoized ! pos
  where
    memoized = mapPos countPaths' grid
    countPaths' _ Empty = 0
    countPaths' (r, _) Beam | r == nrows grid = 1
    countPaths' (r, c) cell | cell `elem` [Start, Beam] = memoized ! (r + 1, c)
    countPaths' (r, c) Splitter = sum [memoized ! (r, c') | c' <- [c - 1, c + 1], c' > 0, c' <= ncols grid]

fireBeam :: Grid -> Grid
fireBeam grid = foldl addBeamsToRow grid [2 .. nrows grid]
  where
    addBeamsToRow grid' r = mapRow addBeam r grid'
      where
        addBeam c Empty
          | grid' ! (r - 1, c) `elem` [Start, Beam] = Beam
          | c > 1 && grid' ! (r, c - 1) == Splitter && grid' ! (r - 1, c - 1) == Beam = Beam
          | c < ncols grid' && grid' ! (r, c + 1) == Splitter && grid' ! (r - 1, c + 1) == Beam = Beam
          | otherwise = Empty
        addBeam _ cell = cell

parseInput :: String -> Grid
parseInput = fromLists . map (map (read . (: []))) . lines

instance Show Cell where
  show Start = "S"
  show Splitter = "^"
  show Beam = "|"
  show Empty = "."

instance Read Cell where
  readsPrec _ ('S' : rest) = [(Start, rest)]
  readsPrec _ ('^' : rest) = [(Splitter, rest)]
  readsPrec _ ('|' : rest) = [(Beam, rest)]
  readsPrec _ ('.' : rest) = [(Empty, rest)]
