import           Data.List.Split

data Present = Present {l :: Int, w :: Int, h :: Int}

parsePresent :: String -> Maybe Present
parsePresent s = createPresent [read x :: Int | x <- splitOn "x" s]
  where
    createPresent [l, w, h] = Just Present {l = l, w = w, h = h}
    createPresent _         = Nothing

requiredWrappingPaper :: Maybe Present -> Int
requiredWrappingPaper (Just p) = surface p + minSurface p
requiredWrappingPaper Nothing  = 0

requiredRibbon :: Maybe Present -> Int
requiredRibbon (Just p) = volume p + add (shortEdges p)
  where
    add (a, b) = 2 * a + 2 * b
requiredRibbon Nothing = 0

surface :: Present -> Int
surface (Present l w h) = 2 * l * w + 2 * w * h + 2 * h * l

volume :: Present -> Int
volume (Present l w h) = l * w * h

shortEdges :: Present -> (Int, Int)
shortEdges (Present l w h)
  | l >= w && l >= h = (h, w)
  | w >= l && w >= h = (h, l)
  | h >= l && h >= w = (w, l)
  | otherwise = (0, 0)

minSurface :: Present -> Int
minSurface = mul . shortEdges where mul (a, b) = a * b

part1 :: [Maybe Present] -> Int
part1 = sum . map requiredWrappingPaper

part2 :: [Maybe Present] -> Int
part2 = sum . map requiredRibbon

parseFile :: String -> [Maybe Present]
parseFile s = map parsePresent $ splitOn "\n" s

part1Str = show . part1 . parseFile

part2Str = show . part2 . parseFile

main = do
  f <- readFile "input.txt"
  putStrLn $ "Required wrapping papper: " ++ part1Str f
  putStrLn $ "Required ribbon: " ++ part2Str f
