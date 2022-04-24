import Control.Exception (evaluate)

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
evaluateRecipe is = let cs = sum [c * q | (Ingredient _ c _ _ _ _, q) <- is]
                        ds = sum [d * q | (Ingredient _ _ d _ _ _, q) <- is]
                        fs = sum [f * q | (Ingredient _ _ _ f _ _, q) <- is]
                        ts = sum [t * q | (Ingredient _ _ _ _ t _, q) <- is]
                        isValid = all (>0) [cs, ds, fs, ts]
                    in if isValid then cs * ds * fs * ts else -1

evaluateRecipe' :: [(Ingredient, Int)] -> Int
evaluateRecipe' is = let cs = sum [c * q | (Ingredient _ c _ _ _ _, q) <- is]
                         ds = sum [d * q | (Ingredient _ _ d _ _ _, q) <- is]
                         fs = sum [f * q | (Ingredient _ _ _ f _ _, q) <- is]
                         ts = sum [t * q | (Ingredient _ _ _ _ t _, q) <- is]
                         cals = sum [cal * q | (Ingredient _ _ _ _ _ cal, q) <- is]
                         isValid = all (>0) [cs, ds, fs, ts] && cals == 500
                    in if isValid then cs * ds * fs * ts else -1

sumTo :: Int
sumTo = 100

sumTo100Perms :: [[Int]]
sumTo100Perms = go 1 (concat [f [i] | i <- [1..sumTo]])
  where
    go 4 acc = acc -- We only have 4 ingredients
    go pos acc = go (pos + 1) (concatMap f acc)
    f xs | length xs == 3 = [xs ++ [sumTo - sum xs]]
         | length xs == 4 = [xs]
         | otherwise = [xs ++ [i] | i <- [1..sumTo - sum xs]] 

-- Part 1 evaluate functions
evaluatePerm :: [Ingredient] -> [Int] -> Int
evaluatePerm is = evaluateRecipe . zip is

evaluatePerms :: [Ingredient] -> [[Int]] -> [Int]
evaluatePerms is = map (evaluatePerm is)

-- Part2 eval functions
evaluatePerm' :: [Ingredient] -> [Int] -> Int
evaluatePerm' is = evaluateRecipe' . zip is

evaluatePerms' :: [Ingredient] -> [[Int]] -> [Int]
evaluatePerms' is = map (evaluatePerm' is)

part1 :: String -> Int
part1 = maximum . flip evaluatePerms sumTo100Perms . parseFile

part2 :: String -> Int
part2 = maximum . flip evaluatePerms' sumTo100Perms . parseFile

main :: IO ()
main = do
  f <- readFile "input.txt"
  let is = parseFile f
  print $ "Part 1 best score: " ++ show (part1 f)
  print $ "Part 2 best score: " ++ show (part2 f)
