import qualified Data.Map as Map
import Data.Char
import Data.List.Split (splitOn)
import Data.List (nub)

type Replacements = Map.Map String [String]

parseFile:: String -> (Replacements, String)
parseFile s = (foldl go Map.empty (map (splitOn " => ") ls), last ls)
  where
    ls = lines s
    go m [fr, to] = Map.unionWith (++) m (Map.singleton fr [to])
    go m _ = m

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop 1 xs)

unchunk :: Int -> [[a]] -> [a]
unchunk n = foldl1 (\acc s -> acc ++ drop (n - 1) s)

indices :: Eq a => a -> [a] -> [Int]
indices x xs = foldl f [] (zip [0..] xs)
  where
    f acc (i, x') = if x' == x then i : acc else acc

doReplacement :: String -> String -> String -> [String]
doReplacement s from to = map replaceSingle (indices from cs)
  where
    cl = length from
    cs = chunks cl s
    frontUnchunk cs' = if null cs' then [] else take (length cs' - cl + 2) (unchunk cl cs')
    rearUnchunk cs' = if null cs' then [] else drop (cl - 1) (unchunk cl cs')
    replaceSingle i = case splitAt i cs of
        (x, _: ys) -> frontUnchunk x ++ to ++ rearUnchunk ys
        (x, y) -> unchunk cl (x ++ y)

applyReplacements::(Replacements, String) -> [String]
applyReplacements (rs, mol) = nub (go (Map.toList rs))
  where
    go [] = []
    go ((from, tos):xs) = concatMap (doReplacement mol from) tos ++ go xs

-- https://www.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4etju/?utm_source=share&utm_medium=web2x&context=3
makeMolecule :: String -> Int
makeMolecule mol = nelems mol - rnArElems - 2 * yElems - 1 
  where
    yElems = length (filter (== 'Y') mol)
    rnArElems = 2 * length (filter (== "Rn") (chunks 2 mol))
    nelems [] = 0
    nelems [x] = 1
    nelems (x:y:xs) | isUpper x && isUpper y = 1 + nelems (y:xs)
                    | isUpper x && isLower y = 1 + nelems xs
                    | otherwise = nelems xs

part1 :: String -> Int
part1 = length . applyReplacements . parseFile

part2 :: String -> Int
part2 = makeMolecule . snd . parseFile

main :: IO ()
main = do
  f <- readFile "input.txt"
  print $ "Part 1 # Molecules: " ++ show (part1 f)
  print $ "Part 2 min steps to molecule: " ++ show (part2 f)
