-- See https://adventofcode.com/2025/day/1.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

-- A position on the dial.
type Position = Int

-- An amount to rotate by. Negative values move left, positive moves right.
type Rotation = Int

main = do
  input <- readFile "./Day1.input.txt"
  let rotations = parseInput input
  print $ part1 rotations
  print $ part2 rotations

part1 :: [Rotation] -> Int
part1 = length . filter (== 0) . positions

part2 :: [Rotation] -> Int
part2 rs = sum $ zipWith countZeroes (positions rs) rs
  where
    countZeroes pos rotation
      -- Right rotation.
      | rotation >= 0 = (pos + rotation) `div` 100
      -- Left rotation. Invert the logic into the equivalent right rotation.
      | otherwise = (((100 - pos) `mod` 100) - rotation) `div` 100

-- Takes a list of rotations and produces a list of cumulative positions after
-- each rotation.
positions :: [Rotation] -> [Position]
positions = scanl turn 50
  where
    turn pos rotation = (pos + rotation) `mod` 100

parseInput :: String -> [Rotation]
parseInput = map readRotation . lines
  where
    readRotation ('L' : n) = -read n
    readRotation ('R' : n) = read n
