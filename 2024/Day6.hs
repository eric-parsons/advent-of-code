import Data.List
import Data.Maybe
import Prelude hiding (Left, Right)

-- See https://adventofcode.com/2024/day/6.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

data Direction = Up | Down | Left | Right deriving (Eq)

data SpaceType = Obstacle | Empty deriving (Eq)

-- For a given space, track all the directions that the guard was facing when
-- they entered the space (they can enter the same space multiple times from
-- different directions).
type Visited = [Direction]

data Space = Space SpaceType Visited deriving (Eq)

-- (row, col) where (0,0) is the top left of the map.
type Coords = (Int, Int)

-- Current position and facing of the guard.
type Guard = (Coords, Direction)

data Map = Map
  { spaces :: [[Space]],
    guard :: Maybe Guard
  }
  deriving (Eq)

main = do
  input <- readFile "./Day6.input.txt"
  let m = parseMap input
  print $ part1 m
  -- This will take several minutes, but eventually does spit out the correct
  -- answer. As stated above, optimization isn't a focus.
  print $ part2 m

part1 = length . concatMap (filter (\(Space _ vs) -> vs /= [])) . spaces . patrol

part2 m = length . filter hasCycle $ mapsWithNewObstacle
  where
    -- If the guard is still on the map after patrolling, they are in a loop.
    hasCycle m = case patrol m of
      (Map {guard = Nothing}) -> False
      _ -> True
    mapsWithNewObstacle = nub . mapMaybe (\(coord, dir) -> tryAddObstacle coord dir m) $ visited
    tryAddObstacle coord dir = trySetSpace (Space Obstacle []) (getNeighbor coord dir)
    -- Calculate the coordinates + direction of each space visited in the
    -- original patrol.
    visited = concat . zipWith (\r row -> concat . zipWith (\c (Space _ vs) -> map ((r, c),) vs) [0 ..] $ row) [0 ..] . spaces $ patrolled
    patrolled = patrol m

-- Moves the guard until they either move off the map or get stuck in a loop.
patrol :: Map -> Map
patrol m = case move m of
  (Just m') -> patrol m'
  Nothing -> m

-- Moves/turns the guard based on what is in front of them. If the guard has
-- already left the map or would begin repeating the same route, returns
-- Nothing instead.
move :: Map -> Maybe Map
move m = do
  (coords, dir) <- guard m
  let nextCoords = getNeighbor coords dir
  case tryGetSpace nextCoords m of
    -- Next space moves off the map, so remove the guard from map.
    Nothing -> Just $ m {guard = Nothing}
    (Just (Space Empty vs))
      -- If the guard is about to visit a space that was already visited
      -- travelling in the same direction, this is the beginning of a cycle.
      -- Prevent any further movement.
      | dir `elem` vs -> Nothing
      | otherwise -> Just $ updateGuard (nextCoords, dir) m
    (Just (Space Obstacle _)) -> Just $ updateGuard (coords, turnRight dir) m

updateGuard :: Guard -> Map -> Map
updateGuard g@(coords, dir) m = m' {guard = Just g}
  where
    (Space _ vs) = getSpace coords m
    -- Update visited info at new space.
    m' = setSpace (Space Empty (dir : vs)) coords m

getSpace :: Coords -> Map -> Space
getSpace (r, c) (Map {spaces}) = spaces !! r !! c

tryGetSpace :: Coords -> Map -> Maybe Space
tryGetSpace coords m
  | areValidCoords coords m = Just $ getSpace coords m
  | otherwise = Nothing

setSpace :: Space -> Coords -> Map -> Map
setSpace s' (row, col) m@(Map {spaces}) = m {spaces = setRow row spaces}
  where
    setRow 0 (r : rs) = setCol col r : rs
    setRow n (r : rs) = r : setRow (n - 1) rs
    setCol 0 (_ : cs) = s' : cs
    setCol n (c : cs) = c : setCol (n - 1) cs

trySetSpace :: Space -> Coords -> Map -> Maybe Map
trySetSpace s' coords m
  | areValidCoords coords m = Just $ setSpace s' coords m
  | otherwise = Nothing

getNeighbor :: Coords -> Direction -> Coords
getNeighbor (r, c) Up = (r - 1, c)
getNeighbor (r, c) Down = (r + 1, c)
getNeighbor (r, c) Left = (r, c - 1)
getNeighbor (r, c) Right = (r, c + 1)

areValidCoords (r, c) (Map {spaces}) = r >= 0 && r < length spaces && c >= 0 && c < length (head spaces)

turnRight Up = Right
turnRight Down = Left
turnRight Left = Up
turnRight Right = Down

parseMap input = setSpace (Space Empty [dir]) coord $ Map {guard = Just guard, spaces = spaces}
  where
    guard = findGuard 0 0 rows
    (coord, dir) = guard
    spaces = map (map parseSpace) rows
    rows = lines input
    parseSpace c = Space (if c == '#' then Obstacle else Empty) []
    findGuard r c ((x : cs) : rs) | x `elem` "^v<>" = ((r, c), read [x])
    findGuard r c ((_ : cs) : rs) = findGuard r (c + 1) (cs : rs)
    findGuard r c ([] : rs) = findGuard (r + 1) 0 rs

instance Read Direction where
  readsPrec _ (c : rest)
    | c == '^' = [(Up, rest)]
    | c == 'v' = [(Down, rest)]
    | c == '<' = [(Left, rest)]
    | c == '>' = [(Right, rest)]
    | otherwise = []
  readsPrec _ [] = []
