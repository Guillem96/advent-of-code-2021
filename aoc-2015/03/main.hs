import           Data.Set as Set

type Point = (Int, Int)

move :: Char -> Point -> Point
move '>' (x, y) = (x + 1, y)
move '<' (x, y) = (x - 1, y)
move '^' (x, y) = (x, y - 1)
move 'v' (x, y) = (x, y + 1)
move _ p        = p

part1 :: Point -> Set Point -> String -> Int
part1 pos set (i : is) = part1 nextPos newSet is
  where
    nextPos = move i pos
    newSet = Set.insert pos set
part1 pos set [] = size newSet where newSet = Set.insert pos set

part2 :: Point -> Point -> Int -> Set Point -> String -> Int
part2 santaPos roboPos turn set (i : is) =
  part2 nextSantaPos nextRoboPos nextTurn newSet is
  where
    nextSantaPos = if even turn then move i santaPos else santaPos
    nextRoboPos = if odd turn then move i roboPos else roboPos
    newSet = if even turn then Set.insert santaPos set else Set.insert roboPos set
    nextTurn = turn + 1
part2 santaPos roboPos turn set [] = size newSet
  where
    newSet = if even turn then Set.insert santaPos set else Set.insert roboPos set

part1Str = show . part1 (0, 0) Set.empty

part2Str = show . part2 (0, 0) (0, 0) 0 Set.empty

main = do
  f <- readFile "input.txt"
  putStrLn $  "Part 1 at least one gift houses: " ++ part1Str f
  putStrLn $  "nPart 1 at least one gift houses: " ++ part2Str f
