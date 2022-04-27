import           Data.List (nub, sort, (\\))
import qualified Data.Map  as Map

data Container = Container {cap::Int, iden::Int} deriving (Eq, Show, Ord)

sumTo :: Int
sumTo = 150

sumContainers :: [Container] -> Int
sumContainers = sum . map cap

sumTo150Perms :: [Container] -> [[Container]]
sumTo150Perms cs = map fst $ go 0 [([c], True) | c <- cs]
  where
    go pos acc | pos == length cs = acc
               | otherwise = go (pos + 1) (concatMap (expand . fst) (filter snd acc))
    expand xs | sumContainers xs > sumTo = [(xs, False)]
              | length xs == length cs = [(xs, sumContainers xs == sumTo)]
              | sumContainers xs == sumTo = [(xs, True)]
              -- Just continue expansion with smaller containers
              | otherwise = [(xs ++ [c], True) | c <- cs \\ xs, c <= last xs]

readContainers :: String -> [Container]
readContainers s = go [] Map.empty cs
  where
    cs = map (\x -> read x :: Int) (lines s)
    go acc cnt [] = acc
    go acc cnt (c:cs) = go (Container c val : acc) ncnt cs
      where
        val = Map.findWithDefault 0 c cnt
        ncnt = Map.insert c (val + 1) cnt

part1 :: String -> Int
part1 = length . sumTo150Perms . readContainers

part2 :: String -> Int
part2 f = let cs = readContainers f
              perms = sumTo150Perms cs
              minl = minimum (map length perms)
          in length (filter (\x -> length x == minl) perms)

main :: IO ()
main = do
    f <- readFile "input.txt"
    print $ "Part 1 number of combinations: " ++ show (part1 f)
    print $ "Part 2 number of combinations: " ++ show (part2 f)
