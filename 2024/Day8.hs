import Data.List.Extra

-- See https://adventofcode.com/2024/day/8.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

-- (row, col) where (0,0) is the top left of the map.
type Coords = (Int, Int)

-- (width, height).
type Dimensions = (Int, Int)

data Map = Map
  { dimensions :: Dimensions,
    antennas :: [(Char, Coords)]
  }
  deriving (Show)

main = do
  input <- readFile "./Day8.input.txt"
  let m = parseMap input
  print $ part1 m
  print $ part2 m

part1 = countWith dualAntinodes

part2 = countWith allAntinodes

-- Counts the antinodes on the map using the specified callback to calculate
-- the antinodes.
countWith :: (Dimensions -> [Coords] -> [Coords]) -> Map -> Int
countWith antinodes (Map {dimensions, antennas}) = length . nub . concatMap (antinodes dimensions) $ antennaGroups
  where
    antennaGroups = map (map snd) . groupSortOn fst $ antennas

dualAntinodes :: Dimensions -> [Coords] -> [Coords]
dualAntinodes dimensions antennas = concat [antinodes a1 a2 | a1 <- antennas, a2 <- antennas, a1 /= a2]
  where
    antinodes (r1, c1) (r2, c2) = filter (areValidCoords dimensions) [(r1 - dr, c1 - dc), (r2 + dr, c2 + dc)]
      where
        dr = r2 - r1
        dc = c2 - c1

allAntinodes :: Dimensions -> [Coords] -> [Coords]
allAntinodes dimensions antennas = concat [antinodes a1 a2 | a1 <- antennas, a2 <- antennas, a1 /= a2]
  where
    antinodes (r1, c1) (r2, c2) = desc ++ asc
      where
        dr = r2 - r1
        dc = c2 - c1
        desc = takeWhile (areValidCoords dimensions) $ zip [r1 - dr, r1 - 2 * dr ..] [c1 - dc, c1 - 2 * dc ..]
        asc = takeWhile (areValidCoords dimensions) $ zip [r1, r1 + dr ..] [c1, c1 + dc ..]

areValidCoords :: Dimensions -> Coords -> Bool
areValidCoords (w, h) (r, c) = r >= 0 && r < w && c >= 0 && c < h

parseMap :: String -> Map
parseMap input = Map {dimensions = (length $ head rows, length rows), antennas = parseAntennas 0 0 rows}
  where
    rows = lines input
    parseAntennas r c (('.' : xs) : rest) = parseAntennas r (c + 1) (xs : rest)
    parseAntennas r c ((x : xs) : rest) = (x, (r, c)) : parseAntennas r (c + 1) (xs : rest)
    parseAntennas r c ([] : rest) = parseAntennas (r + 1) 0 rest
    parseAntennas _ _ [] = []
