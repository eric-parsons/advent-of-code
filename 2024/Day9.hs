import Data.Char
import Data.List.Extra

-- See https://adventofcode.com/2024/day/9.
-- Assumes valid input. Focus is on brevity and simplicity over optimization.

data Block = File Int | FreeSpace deriving (Eq, Show)

type Disk = [Block]

main = do
  input <- readFile "./Day9.input.txt"
  let disk = parseDisk input
  print $ part1 disk
  print $ part2 disk

part1 = checksum . compactBlocks

part2 = checksum . compactFiles

compactBlocks :: Disk -> Disk
compactBlocks disk = take (length . filter isFile $ disk) $ doCompact disk (reverse disk)
  where
    doCompact (FreeSpace : bs) (f@(File _) : rs) = f : doCompact bs rs
    doCompact bs (FreeSpace : rs) = doCompact bs rs
    doCompact (f@(File _) : bs) rs = f : doCompact bs rs
    doCompact [] _ = []

compactFiles :: Disk -> Disk
compactFiles disk = foldl doCompact disk files
  where
    files = reverse . group . filter isFile $ disk
    doCompact d f = let bf = map (const FreeSpace) f in case stripInfix bf d of
        (Just (prefix, suffix)) | f `isInfixOf` suffix -> prefix ++ f ++ replace f bf suffix
        _ -> d

checksum :: Disk -> Int
checksum = sum . zipWith (*) [0..] . map fileId
  where
    fileId (File id) = id
    fileId _ = 0

isFile FreeSpace = False
isFile _ = True

parseDisk :: String -> Disk
parseDisk = parseFile 0 . filter isDigit
  where
    parseFile id (x : xs) = replicate (read [x]) (File id) ++ parseSpace (id + 1) xs
    parseFile _ [] = []
    parseSpace id (x : xs) = replicate (read [x]) FreeSpace ++ parseFile id xs
    parseSpace _ [] = []
