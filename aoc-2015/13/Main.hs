import           Data.List (maximumBy)

type Happiness = (String, String, Int)
type Table = [String]

happinessFromString :: String -> Happiness
happinessFromString s = let ws = words s
                            p1 = head ws
                            p2 = init (last ws)
                            factor = if "lose" `elem` ws then -1 else 1
                            happiness = read (ws !! 3) :: Int
                        in (p1, p2, factor * happiness)

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : filter (/=x) (unique xs)

parseFile :: String -> [Happiness]
parseFile = map happinessFromString . lines

people :: [Happiness] -> [String]
people hs = unique (concat [[x, y] | (x, y, _) <- hs])

happierWith :: [Happiness] -> [String] -> String -> String
happierWith hs cs p = maximumBy g cs
  where
    diff x = getHappiness hs p x + getHappiness hs x p -- Maximize total change
    g x y = compare (diff x) (diff y)

getHappiness :: [Happiness] -> String -> String -> Int
getHappiness h s1 s2 = g (head (filter eq h))
  where
    g (_, _, x) = x
    eq (x, y, _) = x == s1 && y == s2

tableCost :: [Happiness] -> Table  -> Int
tableCost hs t = go 0
  where
    go i | i == length t = 0
         | otherwise = let lh = getHappiness hs (t !! i) (t !! warp (i - 1))
                           rh = getHappiness hs (t !! i) (t !! warp (i + 1))
                       in lh + rh + go (i + 1)

    warp (-1) = length t - 1
    warp i    = i `mod` length t

assignSits :: [Happiness] -> [(Table, Int)]
assignSits hs = zip assignations costs
  where
    costs = map (tableCost hs) assignations
    assignations = [go p' [] [] | p' <- people hs]
    go p v t | length v == length (people hs) - 1 = t ++ [p]
             | otherwise = let ps = people hs
                               v' = (p : v)
                               notV = filter (`notElem` v') ps
                               happyW = happierWith hs notV p
                           in go happyW v' (t ++ [p])

part1 :: String -> (Table, Int)
part1 = maximumBy (\(_, x) (_, y) -> compare x y) . assignSits . parseFile

part2 :: String -> (Table, Int)
part2 f = let hs = parseFile f
              combinations = zip (repeat "Guillem") (people hs)
              xtra = concat [[(x, y, 0), (y, x, 0)] | (x, y) <- combinations]
              hs' = hs ++ xtra
          in maximumBy (\(_, x) (_, y) -> compare x y) (assignSits hs')

main :: IO ()
main = do
    f <- readFile "input.txt"
    let hs = parseFile f
    putStrLn $ "Part 1 table locations: " ++ show (part1 f)
    putStrLn $ "Part 2 table locations: " ++ show (part2 f)

