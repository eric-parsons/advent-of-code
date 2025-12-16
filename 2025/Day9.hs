import Data.List.Extra

-- See https://adventofcode.com/2025/day/9.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

type Coords = (Int, Int)

type Rect = (Coords, Coords)

type Line = (Coords, Coords)

main = do
  input <- readFile "./Day9.input.txt"
  let tiles = parseInput input
  print $ part1 tiles
  print $ part2 tiles

part1 :: [Coords] -> Int
part1 = maximum . map area . pairs

part2 :: [Coords] -> Int
part2 ts = maximum . map area . filter isAllRedGreen . pairs $ ts
  where
    edges = siblings (ts ++ [head ts])
    isAllRedGreen rect = not . any (`inside` rect) $ edges

area :: Rect -> Int
area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

-- Determines whether any point on a line falls strictly inside of a rectangle
-- (touching or overlapping its edges doesn't count).
inside :: Line -> Rect -> Bool
inside ((lx1, ly1), (lx2, ly2)) ((rx1, ry1), (rx2, ry2))
  | lx1 == lx2 && between lx1 rx1 rx2 && min ly1 ly2 < max ry1 ry2 && max ly1 ly2 > min ry1 ry2 = True
  | ly1 == ly2 && between ly1 ry1 ry2 && min lx1 lx2 < max rx1 rx2 && max lx1 lx2 > min rx1 rx2 = True
  | otherwise = False

between :: Int -> Int -> Int -> Bool
between x a b = x > min a b && x < max a b

-- Produces all unique pairs of elements in a list (without regard for order).
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (p : ps) = [(p, q) | q <- ps] ++ pairs ps

-- Produces all pairs of adjacent items in a list.
siblings :: [a] -> [(a, a)]
siblings (x1 : x2 : xs) = (x1, x2) : siblings (x2 : xs)
siblings _ = []

parseInput :: String -> [Coords]
parseInput = map ((\[x, y] -> (read x, read y)) . splitOn ",") . lines
