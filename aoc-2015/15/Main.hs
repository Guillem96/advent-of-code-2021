data Ingredient = Ingredient {
  name::String,
  capacity::Int,
  durability::Int,
  flavor::Int,
  texture::Int,
  calories::Int
}

ingredientFromLine :: String -> Ingredient
ingredientFromLine l = let ws = words l
                           name = init (head ws)
                           c = read (init (ws !! 2)) :: Int
                           d = read (init (ws !! 4)) :: Int
                           f = read (init (ws !! 6)) :: Int
                           t = read (init (ws !! 8)) :: Int
                           cal = read (ws !! 10) :: Int
                       in Ingredient name c d f t cal

parseFile :: String -> [Ingredient]
parseFile = map ingredientFromLine . lines

evaluateRecipe :: [(Ingredient, Int)] -> Int
evaluateRecipe is = let cs = sum [capacity i * q | (i, q) <- is]
                        ds = sum [durability i * q | (i, q) <- is]
                        fs = sum [flavor i * q | (i, q) <- is]
                        ts = sum [texture i * q | (i, q) <- is]
                        isValid = all (>0) [cs, ds, fs, ts]
                    in if isValid then cs * ds * fs * ts else -1

evaluateRecipe' :: [(Ingredient, Int)] -> Int
evaluateRecipe' is = let cs = sum [capacity i * q | (i, q) <- is]
                         ds = sum [durability i * q | (i, q) <- is]
                         fs = sum [flavor i * q | (i, q) <- is]
                         ts = sum [texture i * q | (i, q) <- is]
                         cals = sum [calories i * q | (i, q) <- is]
                         isValid = all (>0) [cs, ds, fs, ts] && cals == 500
                     in if isValid then cs * ds * fs * ts else -1

sumTo :: Int
sumTo = 100

sumTo100Perms :: [[Int]]
sumTo100Perms = go 0 [[i] | i <- [1..sumTo]]
  where
    go 4 acc = acc -- We only have 4 ingredients
    go pos acc = go (pos + 1) (concatMap expand acc)
    expand xs | length xs == 3 = [xs ++ [sumTo - sum xs]]
              | length xs == 4 = [xs]
              | otherwise = [xs ++ [i] | i <- [1..sumTo - sum xs]]

evaluatePerm :: [Ingredient] -> ([(Ingredient, Int)] -> Int) -> [Int] -> Int
evaluatePerm is f = f . zip is

evaluatePerms :: ([(Ingredient, Int)] -> Int) -> [[Int]] -> [Ingredient] -> [Int]
evaluatePerms f perms is = map (evaluatePerm is f) perms

part1 :: String -> Int
part1 = maximum . evaluatePerms evaluateRecipe sumTo100Perms . parseFile

part2 :: String -> Int
part2 = maximum . evaluatePerms evaluateRecipe' sumTo100Perms . parseFile

main :: IO ()
main = do
  f <- readFile "input.txt"
  let is = parseFile f
  print $ "Part 1 best score: " ++ show (part1 f)
  print $ "Part 2 best score: " ++ show (part2 f)
