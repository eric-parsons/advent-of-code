-- See https://adventofcode.com/2025/day/6.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

import Data.Char
import Data.List
import Data.List.Extra

data Op = Add | Mult deriving (Eq)

type Problem = ([Int], Op)

main = do
  input <- readFile "./Day6.input.txt"
  print $ getTotal parseInput1 input
  print $ getTotal parseInput2 input

getTotal :: (String -> [Problem]) -> String -> Int
getTotal parse = sum . map solve . parse

solve :: Problem -> Int
solve (ns, Add) = sum ns
solve (ns, Mult) = product ns

parseInput1 :: String -> [Problem]
parseInput1 = map (toProblem . reverse) . transpose . map words . lines
  where
    toProblem (op : ns) = (map read ns, read op)

parseInput2 :: String -> [Problem]
parseInput2 input = zip nss ops
  where
    rows = lines input
    nss = map (map read) $ split (all isSpace) $ transpose $ init rows
    ops = map read $ words $ last rows

instance Read Op where
  readsPrec _ ('+' : rest) = [(Add, rest)]
  readsPrec _ ('*' : rest) = [(Mult, rest)]
