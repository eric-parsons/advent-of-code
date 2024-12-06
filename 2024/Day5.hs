import Control.Monad
import Data.Graph
import Data.List.Extra
import Data.Maybe

-- See https://adventofcode.com/2024/day/5.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

type Rule = (Int, Int)

type Update = [Int]

main = do
  input <- readFile "./Day5.input.txt"
  let (rules, updates) = parseInput input
      (validUpdates, invalidUpdates) = partition (`satisfiesRules` rules) updates
  print $ part1 validUpdates
  print $ part2 rules invalidUpdates

part1 = sum . map mid

part2 rules = sum . map (mid . fixUpdate)
  where
    fixUpdate u = sortOn (\p -> fromMaybe 0 $ elemIndex p totalOrdering) u
      where
        -- A total ordering of rules can be obtained by performing a
        -- topological sort on a DAG where each rule represents an edge.
        -- Unfortunately, unlike the sample, the puzzle input I was given has
        -- cycles in the rules, meaning the set of rules as a whole is invalid
        -- and can't be sorted. The rule data only becomes valid when filtered
        -- down to just the rules that apply to a given update.
        totalOrdering = topSort $ buildG (1, 100) applicableRules
        applicableRules = filter (\r -> fst r `elem` u) rules

satisfiesRules :: Update -> [Rule] -> Bool
u `satisfiesRules` rules = all (u `satisfiesRule`) rules
  where
    -- An update satisfies a rule (x,y) if the index of x in the update is less
    -- than the index of y, or if one or both does not appear at all.
    u `satisfiesRule` (x, y) = liftM2 (<) (elemIndex x u) (elemIndex y u) /= Just False

mid xs = xs !! (length xs `div` 2)

parseInput :: String -> ([Rule], [Update])
parseInput input = (parseRules rulesInput, parseUpdates updatesInput)
  where
    [rulesInput, updatesInput] = splitOn "\n\n" input
    parseRules = map toPair . lines
    parseUpdates = map (map read . splitOn ",") . lines
    toPair s = let [x, y] = map read . splitOn "|" $ s in (x, y)
