import           Data.List (maximumBy)
import qualified Data.Map  as Map

data Reindeer = Reindeer {name::String, speed::Int, runTime::Int, rest::Int} deriving (Show)

instance Eq Reindeer where
  (==) (Reindeer n _ _ _) (Reindeer n' _ _ _) = n == n'

instance Ord Reindeer where
  compare (Reindeer n _ _ _) (Reindeer n' _ _ _) = compare n n'

reindeerFromString :: String -> Reindeer
reindeerFromString s = let ws = words s
                           name = head ws
                           speed = read (ws !! 3) :: Int
                           runTime = read (ws !! 6) :: Int
                           rest = read (ws !! 13) :: Int
                       in Reindeer name speed runTime rest

distanceIn :: Int -> Reindeer -> Int
distanceIn secs (Reindeer n s rt r) = let wi = rt + r
                                          (nis, lastIntervalDuration) = secs `divMod` wi
                                          d = s * nis * rt
                                          lid = min lastIntervalDuration rt * s
                                      in d + lid

simulate :: [Reindeer] -> Map.Map Reindeer Int
simulate rs = simulate' rs 0 Map.empty Map.empty

simulate' :: [Reindeer] -> Int -> Map.Map Reindeer Int -> Map.Map Reindeer Int -> Map.Map Reindeer Int
simulate' _ 2503 punc dists = punc
simulate' rs ls punc dists = simulate' rs (ls + 1) punc' dists'
  where
    punc' = Map.unionWith (+) punc leadPunc
    leadPunc = Map.fromList (zip lr (repeat 1))
    lr = mapMaxs dists'
    dists' = Map.fromList $ map simReindeer rs
    simReindeer r@(Reindeer _ s rt rest) = let ct = ls `mod` (rt + rest)
                                               shouldRun = ct < rt
                                               cd = Map.findWithDefault 0 r dists
                                           in if shouldRun then (r, cd + s) else (r, cd)

mapMaxs :: Ord a => Map.Map k a -> [k]
mapMaxs m = map fst (filter g l)
  where
    l = Map.toList m
    mv = maximum (map snd l)
    g (_, x) = x == mv

parseFile :: String -> [Reindeer]
parseFile = map reindeerFromString . lines

part1 :: String -> Int
part1 = maximum . map (distanceIn 2503) . parseFile

part2 :: String -> Int
part2 = maximum . map snd . Map.toList . simulate . parseFile

main :: IO ()
main = do
  f <- readFile "input.txt"
  print $ "Part 1 winning distance: " ++ show (part1 f)
  print $ "Part 2 winning score: " ++ show (part2 f)
