forbidden::[String]
forbidden = ["ab", "cd", "pq", "xy"]

count::Eq a => a -> [a] -> Int
count s = length . filter (== s)

contains3vowels::String -> Bool
contains3vowels ss = sum [count v ss | v <- "aeiou"] >= 3

repeatedLetter::String -> Bool
repeatedLetter [] = False
repeatedLetter [x] = False
repeatedLetter (x:y:xs) = x == y || repeatedLetter (y:xs)

containsForbidden::String -> Bool
containsForbidden [] = False
containsForbidden [x] = False
containsForbidden (x:y:xs) = elem [x, y] forbidden || containsForbidden (y:xs)

isnice::String -> Bool
isnice ss = not (containsForbidden ss) && contains3vowels ss && repeatedLetter ss

chunks::String -> [String]
chunks [] = [[]]
chunks [x] = [[]]
chunks [x, y] = [[x, y]]
chunks (x:y:ns) = [x, y] : chunks nexts
  where
    nexts = if x == y && y == head ns then ns else y:ns

containsRepeatedPair::String -> Bool
containsRepeatedPair s = or [count p chunked >= 2 | p <- chunked]
  where
    chunked = chunks s

containsSandwitch::String -> Bool
containsSandwitch [] = False
containsSandwitch [_] = False
containsSandwitch [_, _] = False
containsSandwitch (x:y:z:ns) = x == z || containsSandwitch (y:z:ns)

isnice'::String -> Bool
isnice' ss = containsRepeatedPair ss && containsSandwitch ss

part1::String -> String
part1 = (++) "Part 1 - Number nice words: " . show . length . filter isnice . lines

part2::String -> String
part2 = (++) "Part 2 - Number nice words: " . show . length . filter isnice' . lines

main = do
  readFile "input.txt" >>= print . part1
  readFile "input.txt" >>= print . part2
