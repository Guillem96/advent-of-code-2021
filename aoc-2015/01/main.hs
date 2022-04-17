part1 :: String -> Int
part1 (x : xs) = upDown x + part1 xs
part1 _        = 0

part2 :: String -> Int -> Int -> Int
part2 (x : xs) floor i = if floor == -1 then i else part2 xs nextFloor nexti
  where
    nextFloor = floor + upDown x
    nexti = i + 1
part2 _ _ _ = -1

part1Str = show . part1

part2Str a = show (part2 a 0 0)

upDown :: Char -> Int
upDown '(' = 1
upDown ')' = -1
upDown _   = 0

main = do
  f <- readFile "input.txt"
  putStrLn $ "Part 1 result: " ++ part1Str f
  putStrLn $ "Part 2 result: " ++ part2Str f
