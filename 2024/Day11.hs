-- See https://adventofcode.com/2024/day/11.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

main = do
  input <- readFile "./Day11.input.txt"
  let stones = map read . words $ input
  print $ part1 stones
  print $ part2 stones

part1 = sum . map (blink 25)

part2 = sum . map (blink 75)

-- blink r n gives the number of resulting stones after blinking r times for a
-- stone with number n incribed on it.
blink :: Int -> Int -> Int
blink 0 n = 1
blink r n
  | r < 50 && n < 10 = cache !! r !! n
  | otherwise = sum . map (blink (r - 1)) $ changeStone n

-- First rule of optimization: throw some cache at it and see if that fixes it
-- (it did 😀). The entry cache !! r !! n is the result of the number n after
-- blinking r times.
cache :: [[Int]]
cache = [[sum . map (blink (r - 1)) $ changeStone n | n <- [0 .. 9]] | r <- [0 .. 49]]

changeStone :: Int -> [Int]
changeStone 0 = [1]
changeStone n
  | even numDigits = let (ls, rs) = splitAt (numDigits `div` 2) ds in [read ls, read rs]
  | otherwise = [n * 2024]
  where
    ds = show n
    numDigits = length ds
