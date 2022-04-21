inmemchars :: String -> Int
inmemchars [] = 0
inmemchars ['"', '"'] = 0
inmemchars ('\\':'x':x:y:xs) = 1 + inmemchars xs
inmemchars ('\\':'"':xs) = 1 + inmemchars xs
inmemchars ('\\':'\\':xs) = 1 + inmemchars xs
inmemchars (x:xs) = 1 + inmemchars xs

encode :: String -> String 
encode [] = []
encode ('"':xs)  = "\\\"" ++ encode xs
encode ('\\':xs)  = "\\\\" ++ encode xs
encode (x:xs) = x : encode xs

wordScore :: String -> Int 
wordScore s = length s - inmemchars (init (drop 1 s))

wordScore' :: String -> Int 
wordScore' s = (length (encode s) + 2) - length s

part1 :: String -> Int
part1 = sum . map wordScore . lines

part2 :: String -> Int
part2 = sum . map wordScore' . lines

main :: IO ()
main = do
    f <- readFile "input.txt"
    print $ "Part 1 result: " ++ show (part1 f)
    print $ "Part 2 result: " ++ show (part2 f)
