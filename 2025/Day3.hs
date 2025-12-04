-- See https://adventofcode.com/2025/day/3.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.
type Battery = Int

type Bank = [Battery]

main = do
  input <- readFile "./Day3.input.txt"
  let banks = parseInput input
  print $ part1 banks
  print $ part2 banks

part1 :: [Bank] -> Int
part1 = sum . map (maxJoltage 2)

part2 :: [Bank] -> Int
part2 = sum . map (maxJoltage 12)

maxJoltage :: Int -> Bank -> Int
maxJoltage numBatteries bank = sum $ zipWith (*) powersOfTen $ pickBatteries numBatteries bank
  where
    powersOfTen = [10 ^ n | n <- [numBatteries - 1, numBatteries - 2 .. 0]]
    pickBatteries 0 _ = []
    pickBatteries n bs = m : pickBatteries (n - 1) (tail $ dropWhile (/= m) bs)
      where
        m = maximum $ take (length bs - n + 1) bs

parseInput :: String -> [Bank]
parseInput = map (map (read . (: []))) . lines
