import           Data.List       (intercalate, maximumBy, minimumBy, sortBy)
import           Data.List.Split
import qualified Data.Set        as Set

type Node = String
type Edge = (Node, Node, Int)
type Graph = [(Node, [(Node, Int)])]
type Path = ([Node], Int)
type Heuristic = (Graph -> Node -> Node -> Node -> Ordering)

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : filter (/= x) (unique xs)

parseEdge :: String -> Edge
parseEdge s = (n1, n2, read cost :: Int)
  where
    [ns, cost] = splitOn " = " s
    [n1, n2] = splitOn " to " ns

showPath :: Path -> String
showPath (ns, c) = intercalate " -> " (reverse ns) ++ " = " ++ show c

comparePath :: Path -> Path -> Ordering
comparePath (_, c1) (_, c2) = compare c1 c2

graphFromString :: String -> Graph
graphFromString es = [(ni, getEdges ni) | ni <- nis]
  where
    nis = unique (concat [[x, y] | (x, y, _) <- edges])
    getEdges s = map (edgeToNeighbor s) $ filter (\(x, y, _) -> x == s || y == s) edges
    edgeToNeighbor ni (x, y, c) = if ni == x then (y, c) else (x, c)
    edges = map parseEdge (lines es)

nodes :: Graph -> [Node]
nodes = map fst

adjacentNodes :: Graph -> Node -> [Node]
adjacentNodes g = map fst . snd . nodeById g

edgeCost :: Graph -> Node -> Node -> Int
edgeCost g x y = snd . head . filter f $ snd $ nodeById g x
  where
    f (id', _) = id' == y

nodeById :: Graph -> String -> (Node, [(Node, Int)])
nodeById g i = head (filter (\(i', _) -> i' == i) g)

pathsBy :: Heuristic -> Graph -> [Path]
pathsBy h g = [go n Set.empty ([], 0) | n <- nodes g]
  where
    go n' v' p' | all (`Set.member` v') (adjacentNodes g n') = addToPath n' p'
                | otherwise = go (nextNode n' v') (n' `Set.insert` v') (addToPath n' p')
    nextNode n v = minimumBy (h g n) (nonvisited n v)
    nonvisited n v = filter (`Set.notMember` v) (adjacentNodes g n)
    addToPath nid ([], _)    = ([nid], 0)
    addToPath nid (n':ns, d) = (nid : n' : ns, d + edgeCost g n' nid)

shortestPathHeuristic :: Heuristic
shortestPathHeuristic g n x y = compare (edgeCost g n x) (edgeCost g n y)

longestPathHeuristic :: Heuristic
longestPathHeuristic g n x y = compare (edgeCost g n y) (edgeCost g n x)

part1 :: String -> Path
part1 = minimumBy comparePath . pathsBy shortestPathHeuristic . graphFromString

part2 :: String -> Path
part2 = maximumBy comparePath . pathsBy longestPathHeuristic . graphFromString

main :: IO ()
main = do
    f <- readFile "input.txt"
    print $ "Shortest path: " ++ showPath (part1 f)
    print $ "Longest path: " ++ showPath (part2 f)
