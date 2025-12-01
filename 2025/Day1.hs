-- See https://adventofcode.com/2025/day/1.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

main = do
  input <- readFile "./Day1.input.txt"
  let rotations = parseInput input
  print $ part1 rotations
  print $ part2 rotations

part1 = countZeroes . turns

-- Lazy brute force approach that splits up each rotation into individual
-- clicks instead of dealing with messy math and all of the edge cases :-). 
part2 = countZeroes . turns . concatMap oneAtATime
  where
    oneAtATime n = replicate (abs n) (signum n)

countZeroes :: [Int] -> Int
countZeroes = length . filter (== 0)

-- Takes a list of rotations and produces a list of cumulative positions after
-- each rotation.
turns :: [Int] -> [Int]
turns = scanl turn 50
  where
    turn pos clicks = (pos + clicks) `mod` 100

-- Left rotations are negative and right rotations are positive.
parseInput :: String -> [Int]
parseInput = map toRotation . lines
  where
    toRotation ('L' : n) = -read n
    toRotation ('R' : n) = read n
