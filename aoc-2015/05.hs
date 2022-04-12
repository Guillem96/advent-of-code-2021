b2i False = 0
b2i True = 1

vowels = "aeiou"
forbidden = ["ab", "cd", "pq", "xy"]

count::Eq a => a -> [a] -> Int
count s = length . filter (== s)

contains3vowels::String -> Bool
contains3vowels ss = sum [count v ss| v <- vowels] >= 3

repeatedLetter::String -> Bool
repeatedLetter [x] = False
repeatedLetter (x:y:xs) = x == y || repeatedLetter (y:xs)

containsForbidden::String -> Bool
containsForbidden [x] = False
containsForbidden (x:y:xs) = elem ([x] ++ [y]) forbidden || containsForbidden (y:xs)

isnice::String -> Bool
isnice ss = not (containsForbidden ss) && contains3vowels ss && repeatedLetter ss

-- chunks:: Int -> String -> [String]
chunks n "" = [[]]
chunks s ss = [take s ss] ++ chunks s (drop s ss)

part1::String -> String
part1 = (++) "Number nice words: " . show . length . filter isnice . lines

main = do
    -- readFile "input.txt" >>= print . part1
    print $ chunks 2 "xyxy" 

