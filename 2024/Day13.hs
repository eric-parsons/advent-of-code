import Data.List.Extra
import Text.Regex.PCRE

-- See https://adventofcode.com/2024/day/13.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

type Button = (Int, Int)

type Prize = (Int, Int)

data Machine = Machine Button Button Prize deriving (Show)

main = do
  input <- readFile "./Day13.input.txt"
  let machines = parseMachines input
  print $ part1 machines
  print $ part2 machines

part1 = sum . map cost

part2 = part1 . map (\(Machine a b (px, py)) -> Machine a b (px + 10000000000000, py + 10000000000000))

cost :: Machine -> Int
cost (Machine (ax, ay) (bx, by) (px, py)) = 3 * a + b
  where
    -- Obtained by solving the system of equations ax * a + bx * b == px and
    -- ay * a + by * b == py.
    a' :: Double = fromIntegral (px * by - py * bx) / fromIntegral (ax * by - ay * bx)
    b' :: Double = (fromIntegral px - fromIntegral ax * a') / fromIntegral bx
    -- Since a button can't be pressed a fractional number of times, the
    -- solution is only valid if a' and b' are both integers.
    (a, b) = if isInteger a' && isInteger b' then (floor a', floor b') else (0, 0)

isInteger x = (fromIntegral . floor) x == x

parseMachines :: String -> [Machine]
parseMachines = map (\[a, b, p] -> Machine (parseButton a) (parseButton b) (parsePrize p)) . splitOn [""] . lines

parseButton :: String -> Button
parseButton input = (read x, read y)
  where
    (_, _, _, [x, y]) = input =~ "Button [AB]: X\\+(\\d+), Y\\+(\\d+)" :: (String, String, String, [String])

parsePrize :: String -> Prize
parsePrize input = (read x, read y)
  where
    (_, _, _, [x, y]) = input =~ "Prize: X=(\\d+), Y=(\\d+)" :: (String, String, String, [String])
