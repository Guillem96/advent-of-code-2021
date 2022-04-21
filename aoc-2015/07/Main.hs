import           Data.Bits
import           Data.Char

import           Data.IORef
import           Data.List
import           Data.List.Split
import qualified Data.Map         as Map
import qualified Data.Word        as Word
import           System.IO.Unsafe

data Op = And | Or | Lshift | Rshift | Not | Idem deriving (Eq, Ord)
data Operand = IntOperand Word.Word16 | SignalOperand String deriving (Eq, Ord)
data SyntaxTree = Node String Op [SyntaxTree] | AnonLeaf Word.Word16

-- Shallow comparison for the memoize function
instance Eq SyntaxTree where
  (==) (Node id1 _ _) (Node id2 _ _) = id1 == id2
  (==) (AnonLeaf v1) (AnonLeaf v2)   = v1 == v2
  (==) _ (AnonLeaf _)                = False
  (==) (AnonLeaf _) _                = False

instance Ord SyntaxTree where
  compare (Node id1 _ _) (Node id2 _ _) = compare id1 id2
  compare (AnonLeaf v1) (AnonLeaf v2)   = compare v1 v2
  compare _ (AnonLeaf _)                = GT
  compare (AnonLeaf _) _                = LT

instance Show Operand where
  show (IntOperand x)    = show x
  show (SignalOperand s) = "Signal:" ++ s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

applyBitWise :: Op -> [Word.Word16] -> Word.Word16
applyBitWise And [x, y]    = (.&.) x y
applyBitWise Or [x, y]     = (.|.) x y
applyBitWise Lshift [x, y] = shiftL x (fromIntegral y)
applyBitWise Rshift [x, y] = shiftR x (fromIntegral y)
applyBitWise Not [x]       = complement x
applyBitWise Idem [x]      = x
applyBitWise _ _           = 0

signalFromString :: String -> (String, Op, [Operand])
signalFromString s
  | "AND" `isInfixOf` s = (id, And, ops "AND")
  | "OR" `isInfixOf` s = (id, Or, ops "OR")
  | "LSHIFT" `isInfixOf` s = (id, Lshift, ops "LSHIFT")
  | "RSHIFT" `isInfixOf` s = (id, Rshift, ops "RSHIFT")
  | "NOT" `isInfixOf` s = (id, Not, ops "NOT")
  | otherwise = (id, Idem, ops " ")
  where
    [opss, id] = splitOn " -> " s
    splitOp o = filter (/= "") (map trim (splitOn o opss))
    ops = map parseOperand . splitOp

parseOperand :: String -> Operand
parseOperand s
  | all isDigit s = IntOperand (read s :: Word.Word16)
  | otherwise = SignalOperand s

memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do let y = f x
                          writeIORef r (Map.insert x y m)
                          return y

buildTrees :: [(String, Op, [Operand])] -> [SyntaxTree]
buildTrees xs = map go xs
  where
    go (id, op, ops) = Node id op (map btm ops)
    btm = memoize buildTree
    buildTree (IntOperand v) = AnonLeaf v
    buildTree (SignalOperand id') = nn
      where
        nn = Node id' nop (map btm nops)
        (_, nop, nops) = nextNode id'
    nextNode i = head (filter g xs) where g (x, _, _) = x == i

msolve :: SyntaxTree -> Word.Word16
msolve = memoize solve

solve :: SyntaxTree -> Word.Word16
solve (AnonLeaf v)    = v
solve (Node _ op ops) = applyBitWise op (map msolve ops)

treeById :: String -> [SyntaxTree] -> SyntaxTree
treeById id ts = head (filter g ts)
  where
    g (Node id' _ _) = id' == id
    g (AnonLeaf v)   = False

part1 :: String -> Word.Word16
part1 = msolve . treeById "a" . buildTrees . map signalFromString . lines

part2 :: Word.Word16 -> String ->  Word.Word16
part2 x = msolve . treeById "a" . buildTrees . replaceValue "b" x . map signalFromString . lines

replaceValue :: String -> Word.Word16 -> [(String, Op, [Operand])] -> [(String, Op, [Operand])]
replaceValue rid rv [] = []
replaceValue rid rv (x@(id, Idem, [IntOperand v]):xs) | id == rid = (id, Idem, [IntOperand rv]) : xs
                                                      | otherwise = x : replaceValue rid rv xs
replaceValue rid rv (x:xs) = x : replaceValue rid rv xs

main = do
  f <- readFile "input.txt"
  -- Can't run both parts at same time due to memoization conflicts
  -- print $ "Part1 A value = " ++ show (part1 f)
  print $ "Part2 B value = " ++ show (part2 3176 f)

