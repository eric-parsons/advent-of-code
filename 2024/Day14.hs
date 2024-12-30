import Data.List.Extra
import Text.Regex.PCRE

-- See https://adventofcode.com/2024/day/14.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

type Position = (Int, Int)

type Velocity = (Int, Int)

type Seconds = Int

data Robot = Robot Position Velocity deriving (Show)

width = 101

height = 103

main = do
  input <- readFile "./Day14.input.txt"
  let robots = parseRobots input
  print $ part1 robots
  -- Renders each candidate frame to the user, requiring a key press to proceed
  -- to the next. For my input, takes ~10 tries to find it.
  let candidateFrames = part2 robots
  mapM (printFrame robots) candidateFrames

part1 :: [Robot] -> Int
part1 = product . map length . group . sort . filter (/= 0) . map (quadrant . advance 100)

-- Produces an infinite list of frame numbers (i.e. seconds elapsed) that are
-- likely to contain an image.
part2 :: [Robot] -> [Seconds]
part2 rs = [s | s <- [1 ..], (density . advanceAll s) rs > 1.0]
  where
    numRobots = length rs
    -- Measures the average number of neighboring robots surrounding each robot.
    density rs = (/ fromIntegral numRobots) . fromIntegral . sum . map (\r -> length $ filter (`isNeighborOf` r) rs) $ rs

-- Renders an image of what the robots look like after the specified number of
-- seconds.
printFrame :: [Robot] -> Seconds -> IO ()
printFrame rs s = do
  print $ Grid $ advanceAll s rs
  print s
  _ <- getChar
  return ()

advance :: Seconds -> Robot -> Robot
advance s (Robot (x, y) (vx, vy)) = Robot ((x + vx * s) `mod` width, (y + vy * s) `mod` height) (vx, vy)

advanceAll :: Seconds -> [Robot] -> [Robot]
advanceAll s = map (advance s)

quadrant :: Robot -> Int
quadrant (Robot (x, y) _)
  | x < mw && y < mh = 1
  | x < mw && y > mh = 2
  | x > mw && y < mh = 3
  | x > mw && y > mh = 4
  -- Indicates it is along a dividing line.
  | otherwise = 0
  where
    mw = width `div` 2
    mh = height `div` 2

-- Two robots are neighbors of one another if they are adjacent or immediately
-- diagonal.
isNeighborOf :: Robot -> Robot -> Bool
isNeighborOf (Robot (x, y) _) (Robot p _) = p `elem` [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

-- Used purely for display purposes.
newtype Grid = Grid [Robot]

instance Show Grid where
  show (Grid rs) = unlines $ map showLine [0 .. height - 1]
    where
      showLine y = map (showSpace y) [0 .. width - 1]
      showSpace y x = case length . filter (\(Robot p _) -> p == (x, y)) $ rs of
        0 -> '.'
        n -> head . show . min 9 $ n

parseRobots :: String -> [Robot]
parseRobots = map parseRobot . lines
  where
    parseRobot line = Robot (read x, read y) (read vx, read vy)
      where
        (_, _, _, [x, y, vx, vy]) = line =~ "p=([-\\d]+),([-\\d]+) v=([-\\d]+),([-\\d]+)" :: (String, String, String, [String])
