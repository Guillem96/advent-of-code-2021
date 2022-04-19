import Data.Bits
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import qualified Data.Word as Word
-- import qualified Data.Int as DataInt

import Data.IORef
import System.IO.Unsafe
data Op = And | Or | Lshift | Rshift | Not | Idem deriving (Eq, Ord)
data Operator = IntOperator Word.Word16 | SignalOperator String deriving (Eq, Ord)
data SyntaxTree = Node String Op [SyntaxTree] | AnonLeaf Word.Word16 deriving (Eq, Ord)

instance Show Op where
  show And = "&"
  show Or = "|"
  show Lshift = "<<"
  show Rshift = ">>"
  show Not = "not"
  show Idem = "idem"

instance Show SyntaxTree where
  show (AnonLeaf v) = show v
  show (Node id op ops@[op1, op2]) =
    shallowShow op1 ++ " " ++ show op ++ " " ++ shallowShow op2 ++ " -> " ++ id 
    ++ "\n---\n" ++ show op1  ++ "\n---\n" ++ show op2
  show (Node id op [o]) =
    show op ++ " " ++ shallowShow o ++ " -> " ++ id ++ "\n" ++ show o
  show (Node id op []) = "<pruned> " ++ id ++ " " ++ show op
  show (Node id op _) = "<unexpected> " ++ id ++ " " ++ show op


shallowShow::SyntaxTree -> String
shallowShow (AnonLeaf v) = show v
shallowShow (Node id _ _) = id

instance Show Operator where
  show (IntOperator x) = show x
  show (SignalOperator s) = "Signal:" ++ s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

applyBitWise :: Op -> [Word.Word16] -> Word.Word16
applyBitWise And [x, y] = (.&.) x y
applyBitWise Or [x, y] = (.|.) x y
applyBitWise Lshift [x, y] = shiftL x (fromIntegral y)
applyBitWise Rshift [x, y] = shiftR x (fromIntegral y)
applyBitWise Not [x] = complement x
applyBitWise Idem [x] = x
applyBitWise _ _ = 0


signalFromString:: String -> (String, Op, [Operator])
signalFromString s | "AND" `isInfixOf` s = (id, And, ops "AND")
                   | "OR" `isInfixOf` s = (id, Or, ops "OR")
                   | "LSHIFT" `isInfixOf` s = (id, Lshift, ops "LSHIFT")
                   | "RSHIFT" `isInfixOf` s = (id, Rshift, ops "RSHIFT")
                   | "NOT" `isInfixOf` s = (id, Not, ops "NOT")
                   | otherwise  = (id, Idem, ops " ")
  where
    [opss, id] = splitOn " -> " s
    splitOp o = filter (/= "") (map trim (splitOn o opss))
    ops = map parseOperator . splitOp

parseOperator:: String -> Operator
parseOperator s | all isDigit s = IntOperator (read s :: Word.Word16)
                | otherwise = SignalOperator s

memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y

buildTrees :: [(String, Op, [Operator])] -> [SyntaxTree]
buildTrees xs = map (memoize go) xs
  where
    go (id, op, ops) = Node id op (map buildTree ops)
    buildTree (IntOperator v) = AnonLeaf v
    buildTree (SignalOperator id') = nn
      where
        nn = Node nid nop (map buildTree nops)
        (nid, nop, nops) = nextNode id'
    nextNode i = head (filter g xs) where g (x, _, _) = x == i


solve :: SyntaxTree -> Word.Word16
solve (AnonLeaf v) = v
solve (Node _ op ops) = applyBitWise op (map solve ops)

solveTrees :: [SyntaxTree] -> Map.Map String Word.Word16
solveTrees ts = Map.fromList [(id, solve (treeById id ts)) | id <- treeVariables ts]

treeById:: String -> [SyntaxTree] -> SyntaxTree
treeById id ts = head (filter g ts)
  where
    g (Node id' _ _) = id' == id
    g (AnonLeaf v) = False
    prune 0 (Node i o _) = Node i o []
    prune n (Node i o ops) = Node i o (map (prune (n - 1)) ops)
    prune _ l = l

treeVariables:: [SyntaxTree] -> [String]
treeVariables = map getId where getId (Node id _ _) = id

parseTrees :: String -> [SyntaxTree]
parseTrees = buildTrees . map signalFromString . lines

part1 :: String -> Word.Word16
part1 = memoize solve . treeById "lx" . parseTrees

main = do
  f <- readFile "input.txt"
  -- putStrLn $ unlines $ map (show . signalFromString) (lines f)
  -- print $ treeVariables $ (parseTrees f)
  print $ part1 f
