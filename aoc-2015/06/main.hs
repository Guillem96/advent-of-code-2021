module Main where

import           CoordCompression (CoordinateCompression2D, compress,
                                   decompress, fromCoords)
import           Data.List.Split  (splitOn)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Position         (Position (..), add, posFromString)

data Action = On | Off | Toggle

data Instruction = Instruction Action Position Position

unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : filter (/= x) (unique xs)

amount :: Action -> Int
amount On     = 1
amount Off    = -1
amount Toggle = 2

addMaps :: Map.Map Position Int -> Map.Map Position Int -> Map.Map Position Int
addMaps = Map.unionWith f where f l r = max (l + r) 0

positionRange :: Position -> Position -> [Position]
positionRange (Position2D x1 y1) (Position2D x2 y2) =
  [ Position2D x y | x <- [x1 .. x2 - 1], y <- [y1 .. y2 - 1] ]

apply :: [Maybe Instruction] -> Set.Set Position -> Set.Set Position
apply is ls = foldl applySingle ls is

applySingle :: Set.Set Position -> Maybe Instruction -> Set.Set Position
applySingle ls (Just (Instruction On p1 p2)) = ls `Set.union` toOpen
  where toOpen = Set.fromList (positionRange p1 p2)
applySingle ls (Just (Instruction Off p1 p2)) = ls `Set.difference` toOff
  where toOff = Set.fromList (positionRange p1 p2)
applySingle ls (Just (Instruction Toggle p1 p2)) =
  (ls `Set.union` toOn) `Set.difference` toOff
 where
  toOn       = toggleArea `Set.difference` ls
  toOff      = ls `Set.intersection` toggleArea
  toggleArea = Set.fromList (positionRange p1 p2)
applySingle ls _ = ls

apply' :: [Maybe Instruction] -> Map.Map Position Int -> Map.Map Position Int
apply' is ls = foldl applySingle' ls is

applySingle'
  :: Map.Map Position Int -> Maybe Instruction -> Map.Map Position Int
applySingle' ls (Just (Instruction op p1 p2)) = Map.filter (> 0) jm
 where
  a  = amount op
  nm = Map.fromList (zip (positionRange p1 p2) (repeat a))
  jm = ls `addMaps` nm
applySingle' ls _ = ls

startsWith' :: Eq a => [a] -> [a] -> Bool
startsWith' s ss = take (length s) ss == s

removePrefix :: Eq a => [a] -> [a] -> [a]
removePrefix p ss | startsWith' p ss = drop (length p) ss
                  | otherwise        = ss

instructionAction :: [Char] -> Action
instructionAction s | startsWith' "turn on" s  = On
                    | startsWith' "toggle" s   = Toggle
                    | startsWith' "turn off" s = Off
                    | otherwise                = Off

instructionsCoords :: [Char] -> (Maybe Position, Maybe Position)
instructionsCoords s
  | startsWith' "turn on" s = parsePos(clean "turn on" s)
  | startsWith' "toggle" s = parsePos (clean "toggle" s)
  | startsWith' "turn off" s = parsePos (clean "turn off" s)
  | otherwise = (Nothing, Nothing)
 where
  clean pref = splitOn "through " . removePrefix pref
  parsePos ps = toTuple [ posFromString p | p <- ps ]
  toTuple [a, b] = (a, b)
  toTuple _      = (Nothing, Nothing)

instructionFromLine :: String -> Maybe Instruction
instructionFromLine s = case instructionsCoords s of
  (Just p1, Just p2) -> Just (Instruction (instructionAction s) p1 p2)
  (_      , _      ) -> Nothing

learnCompressor :: [Maybe Instruction] -> CoordinateCompression2D
learnCompressor is = fromCoords ps
  where ps = concat [ [p1, p2 `add` 1] | (Just (Instruction _ p1 p2)) <- is ]

compressInstructions
  :: [Maybe Instruction] -> (CoordinateCompression2D, [Maybe Instruction])
compressInstructions is = (cc, cis)
 where
  cc  = learnCompressor is
  cis = map compressIns is
  compressIns Nothing = Nothing
  compressIns (Just (Instruction a p1 p2)) =
    Just (Instruction a (compress cc p1) (compress cc (p2 `add` 1)))

decompressArea :: CoordinateCompression2D -> [Position] -> Int
decompressArea cc = foldl f 0 where f acc p = acc + decompressArea' cc p

decompressArea2 :: CoordinateCompression2D -> [(Position, Int)] -> Int
decompressArea2 cc = foldl f 0
  where f acc (p, w) = acc + (decompressArea' cc p * w)

decompressArea' :: CoordinateCompression2D -> Position -> Int
decompressArea' cc p = xrange * yrange
 where
  xrange                 = xmax - xmin
  yrange                 = ymax - ymin
  (Position2D xmin ymin) = decompress cc p
  (Position2D xmax ymax) = decompress cc (p `add` 1)

parseFile :: String -> [Maybe Instruction]
parseFile = map instructionFromLine . lines

part1 :: [Char] -> String
part1 s = "Part1 number of lights: " ++ show area
 where
  area      = decompressArea cc cps
  cps       = Set.toList (apply cis Set.empty)
  (cc, cis) = compressInstructions . parseFile $ s

part2 :: [Char] -> String
part2 s = "Part2 total brightness: " ++ show area
 where
  area      = decompressArea2 cc cps
  cps       = Map.toList (apply' cis Map.empty)
  (cc, cis) = compressInstructions . parseFile $ s

main :: IO ()
main = do 
  f <- readFile "input.txt"
  print $ part1 f
  print $ part2 f
