import Text.Regex.PCRE

-- See https://adventofcode.com/2024/day/3.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

data Token = Enable | Disable | Mul Int Int deriving (Show)

main = do
  input <- readFile "./Day3.input.txt"
  let tokens = parseInput input
  print $ part1 tokens
  print $ part2 tokens

part1 = sum . map eval . filter isMul

part2 = sum . map eval . filterMuls True
  where
    filterMuls True (t : ts) | isMul t = t : filterMuls True ts
    filterMuls True (Disable : ts) = filterMuls False ts
    filterMuls False (Enable : ts) = filterMuls True ts
    filterMuls enabled (t : ts) = filterMuls enabled ts
    filterMuls _ [] = []

isMul (Mul _ _) = True
isMul _ = False

eval (Mul x y) = x * y
eval _ = 0

parseInput input = map toToken matches
  where
    matches = input =~ "mul\\((\\d{1,3}),(\\d{1,3})\\)|don't|do" :: [[String]]
    toToken ("do" : _) = Enable
    toToken ("don't" : _) = Disable
    toToken (_ : x : y : _) = Mul (read x) (read y)
