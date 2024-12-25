import Data.List

-- See https://adventofcode.com/2024/day/12.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

-- (row, col) where (0,0) is the top left of the map.
type Coords = (Int, Int)

type Plot = (Coords, Char)

type Region = [Plot]

main = do
  input <- readFile "./Day12.input.txt"
  let plots = parseInput input
  let regions = makeRegions plots
  print $ part1 regions
  print $ part2 regions

part1 = sum . map price

part2 = sum . map bulkPrice

makeRegions :: [Plot] -> [Region]
makeRegions ps = foldr addPlot [] ps
  where
    addPlot p rs = (p : concat matching) : other
      where
        -- Note that a plot can be adjacent to multiple regions created so far,
        -- causing previously separate regions to join together into a single
        -- one. This is why partition is used to find all matching regions and
        -- concatenate them together.
        (matching, other) = partition (\r -> any (`elem` r) ns) rs
        ns = neighbors p ps

price :: Region -> Int
price r = length r * perimeter r

bulkPrice :: Region -> Int
bulkPrice r = length r * (length . sides) r

perimeter :: Region -> Int
perimeter r = sum . map (\p -> 4 - length (neighbors p r)) $ r

sides :: Region -> [[Coords]]
sides r = lefts ++ rights ++ aboves ++ belows
  where
    -- Get the coordinates of all spaces immediately surrrounding the region, and
    -- group them in each direction using a similar technique used in
    -- makeRegions.

    inside = map fst r
    lefts = getSides groupVertical left
    rights = getSides groupVertical right
    aboves = getSides groupHorizontal above
    belows = getSides groupHorizontal below
    getSides groupSides adjacent = foldr groupSides [] . nub . filter (not . (`elem` inside)) . map adjacent $ inside
    groupVertical (r, c) sides' = ((r, c) : concat matching) : other
      where
        (matching, other) = partition (any (\(r', c') -> c' == c && abs (r' - r) == 1)) sides'
    groupHorizontal (r, c) sides' = ((r, c) : concat matching) : other
      where
        (matching, other) = partition (any (\(r', c') -> r' == r && abs (c' - c) == 1)) sides'

-- Gets plots adjacent to the given plot within a set of plots that belong to
-- the same region.
neighbors :: Plot -> [Plot] -> [Plot]
neighbors (coords, x) ps = filter (`elem` ps) [(left coords, x), (right coords, x), (above coords, x), (below coords, x)]

left (r, c) = (r, c - 1)

right (r, c) = (r, c + 1)

above (r, c) = (r - 1, c)

below (r, c) = (r + 1, c)

parseInput :: String -> [Plot]
parseInput = parsePlots 0 0 . lines
  where
    parsePlots r c ((x : xs) : rest) = ((r, c), x) : parsePlots r (c + 1) (xs : rest)
    parsePlots r c ([] : rest) = parsePlots (r + 1) 0 rest
    parsePlots _ _ [] = []
