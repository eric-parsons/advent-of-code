import Data.List.Extra

-- See https://adventofcode.com/2024/day/10.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

-- (row, col) where (0,0) is the top left of the map.
type Coords = (Int, Int)

type Height = Int

type Space = (Coords, Height)

type Map = [Space]

type Trail = [Space]

main = do
  input <- readFile "./Day10.input.txt"
  let ts = trails $ parseMap input
  print $ part1 ts
  print $ part2 ts

part1 :: [Trail] -> Int
part1 = sum . map (length . nub . map last) . groupSortOn head

part2 :: [Trail] -> Int
part2 = length

-- Gets all distinct trails in the map starting at height 0 and ending at
-- height 9.
trails :: Map -> [Trail]
trails m = iterate (>>= expand) trailheads !! 9
  where
    trailheads = map (: []) . filter ((== 0) . snd) $ m
    expand trail = [trail ++ [s] | s <- nextSteps m (last trail)]

-- Gets all of the possible next spaces from a given space within a map.
nextSteps :: Map -> Space -> [Space]
nextSteps m ((r, c), h) = filter (`elem` m) [((r + 1, c), h'), ((r - 1, c), h'), ((r, c - 1), h'), ((r, c + 1), h')]
  where
    h' = h + 1

parseMap :: String -> Map
parseMap = parseHeights 0 0 . lines
  where
    parseHeights r c ((x : xs) : rest) = ((r, c), read [x]) : parseHeights r (c + 1) (xs : rest)
    parseHeights r c ([] : rest) = parseHeights (r + 1) 0 rest
    parseHeights _ _ [] = []
