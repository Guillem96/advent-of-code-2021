import qualified Data.Set as Set

type Pos = (Int, Int)
type Board = Set.Set Pos

width :: Int
width = 100

height :: Int
height = 100

corners :: Set.Set (Int, Int)
corners = Set.fromList [(0, 0), (0, height - 1), (width - 1, 0), (width - 1, height - 1)]

showBoard :: Board -> String
showBoard b = go (0, 0)
  where
    go (x, y) | y == height = ""
              | x == width = "\n" ++ go (0, y + 1)
              | isOn b (x, y) = "#" ++ go (x + 1, y)
              | otherwise = "." ++ go (x + 1, y)

readBoard :: String -> Board
readBoard s = Set.fromList $ concatMap fetchPoses (zip [0..] (lines s))
  where
    fetchPoses (y, l) = zip (indices '#' l) (repeat y)

indices :: Eq a => a -> [a] -> [Int]
indices x xs = foldl f [] (zip [0..] xs)
  where
    f acc (i, x') = if x' == x then i : acc else acc

isOn :: Board -> Pos -> Bool
isOn b p = p `Set.member` b

isOff :: Board -> Pos -> Bool
isOff b p = not (isOn b p)

neighs :: Pos -> [Pos]
neighs (x, y) = filter g [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                          (x - 1, y),                 (x + 1, y),
                          (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
  where
    g (x, y) = x >= 0 && x < width && y >= 0 && y < height


onnNeighs :: Board -> Pos -> Int
onnNeighs b = length . filter (isOn b) . neighs

survivors :: Board -> Board
survivors b = Set.fromList [p | p <- Set.toList b, onnNeighs b p `elem` [2, 3]]

births :: Board -> Board
births b = Set.fromList [(x, y) | x <- [0..width - 1],
                                  y <- [0..height - 1],
                                  isOff b (x, y),
                                  onnNeighs b (x, y) == 3]

step :: Board -> Board
step b = survivors b `Set.union` births b

step' :: Board -> Board
step' b = corners `Set.union` (survivors b `Set.union` births b)

part1 :: String -> Int
part1 = length . (!! 100) . iterate step . readBoard

part2 :: String -> Int
part2 = length . (!! 100) . iterate step' . (Set.union corners . readBoard)

main :: IO ()
main = do
    f <- readFile "input.txt"
    let b = readBoard f
    print $ "Part 1 lights on: " ++ show (part1 f)
    print $ "Part 2 lights on: " ++ show (part2 f)
