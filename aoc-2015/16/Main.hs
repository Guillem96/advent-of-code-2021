import           Data.List.Split (splitOn)
import qualified Data.Map        as Map

type Aunt = Map.Map String Int

findings :: Map.Map String Int
findings = Map.fromList [("children", 3),
                         ("cats", 7),
                         ("samoyeds", 2),
                         ("pomeranians", 3),
                         ("akitas", 0),
                         ("vizslas", 0),
                         ("goldfish", 5),
                         ("trees", 3),
                         ("cars", 2),
                         ("perfumes", 1)]

auntFromLine :: String -> Aunt
auntFromLine = Map.fromList . props . dropPrefix
  where
    dropPrefix l =  dropWhile (== ' ') (drop 8 l)
    props = map parseProp . splitOn ", "
    parseProp s = case splitOn ": " s of
                    [x, y] -> (x, read y :: Int)
                    _      -> ("", -1)

auntMatches :: Aunt -> Bool
auntMatches = and . Map.elems . Map.intersectionWith (==) findings

auntMatches' :: Aunt -> Bool
auntMatches' = and . Map.elems . Map.intersectionWithKey f findings
  where
    f "cats"        = (<)
    f "trees"       = (<)
    f "pomeranians" = (>)
    f "goldfish"    = (>)
    f _             = (==)

parseFile :: String -> [Aunt]
parseFile = map auntFromLine . lines

part1 :: String -> [(Integer, Aunt)]
part1 = filter (\(_, a) -> auntMatches a) . zip [1..] . parseFile

part2 :: String -> [(Integer, Aunt)]
part2 = filter (\(_, a) -> auntMatches' a) . zip [1..] . parseFile

main :: IO ()
main = do
    f <- readFile "input.txt"
    print $ "Part 1 aunt: " ++ show (part1 f)
    print $ "Part 2 aunt: " ++ show (part2 f)
