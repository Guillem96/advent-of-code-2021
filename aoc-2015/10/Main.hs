next :: String -> String
next [] = []
next xs = show (length rep) ++ [digit] ++ next nxs
  where
    rep = takeWhile (== head xs) xs
    nxs = drop (length rep) xs
    digit = head rep

part1 :: Int
part1 = length $ foldl (\acc _ -> next acc) "1321131112" [1..40]

part2 :: Int
part2 = length $ foldl (\acc _ -> next acc) "1321131112" [1..50]

main :: IO ()
main = do
    print $ "Part 1: " ++ show part1
    print $ "Part 1: " ++ show part2
