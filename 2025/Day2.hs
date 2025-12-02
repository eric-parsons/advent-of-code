-- See https://adventofcode.com/2025/day/2.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

type Range = (Int, Int)

main = do
  input <- readFile "./Day2.input.txt"
  let ranges = parseInput input
  print $ part1 ranges
  print $ part2 ranges

part1 :: [Range] -> Int
part1 = countInvalid isInvalid1

part2 :: [Range] -> Int
part2 = countInvalid isInvalid2

countInvalid :: (Int -> Bool) -> [Range] -> Int
countInvalid isInvalid = sum . concatMap (invalidIds isInvalid)

invalidIds :: (Int -> Bool) -> Range -> [Int]
invalidIds isInvalid (min, max) = [id | id <- [min .. max], isInvalid id]

isInvalid1 :: Int -> Bool
isInvalid1 id = let (h1, h2) = splitAt (len `div` 2) str in h1 == h2
  where
    str = show id
    len = length str

isInvalid2 :: Int -> Bool
-- Take the first half, third, quarter, etc. and see if repeating it 2, 3, 4,
-- etc. times equals the original number.
isInvalid2 id = str `elem` [concat $ replicate n sub | n <- [2 .. len], let (sub, _) = splitAt (len `div` n) str]
  where
    str = show id
    len = length str

parseInput :: String -> [Range]
parseInput = map readRange . split ','
  where
    readRange str = case split '-' str of [min, max] -> (read min, read max)

split :: Eq a => a -> [a] -> [[a]]
split x xs = case break (== x) xs of
  (ys, x' : zs) -> ys : split x zs
  (ys, []) -> [ys]
