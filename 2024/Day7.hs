import Control.Monad
import Data.List.Extra
import Data.Maybe

-- See https://adventofcode.com/2024/day/6.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

data Token = Add | Mul | Concat | Val Int deriving (Show)

type Equation = (Int, [Int])

main = do
  input <- readFile "./Day7.input.txt"
  let eqs = parseInput input
  print $ part1 eqs
  print $ part2 eqs

part1 = sum . map fst . filter (isValidWith [Add, Mul])

part2 = sum . map fst . filter (isValidWith [Add, Mul, Concat])

isValidWith :: [Token] -> Equation -> Bool
isValidWith ops (total, xs) = any (\combo -> eval (alternate (map Val xs) combo) == total) opCombinations
  where
    opCombinations = replicateM (length xs - 1) ops

eval :: [Token] -> Int
eval ((Val x) : Add : (Val y) : rest) = eval $ Val (x + y) : rest
eval ((Val x) : Mul : (Val y) : rest) = eval $ Val (x * y) : rest
eval ((Val x) : Concat : (Val y) : rest) = eval $ Val (read $ show x ++ show y) : rest
eval [Val x] = x

-- Joins two lists by alternating between their elements.
alternate (x : xs) ys = x : alternate ys xs
alternate _ ys = ys

parseInput :: String -> [Equation]
parseInput = map parseLine . lines
  where
    parseLine xs = let (prefix, suffix) = fromJust . stripInfix ": " $ xs in (read prefix, map read . words $ suffix)
