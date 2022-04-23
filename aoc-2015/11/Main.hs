import           Data.Char (chr, ord)
import           Data.List


catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

minJust :: Ord a => [Maybe a] -> Maybe a
minJust xs = case catMaybes xs of
              [] -> Nothing
              xs -> Just (minimum xs)

containsIncreasing :: String -> Bool
containsIncreasing x = go 0 x
  where
    go acc [] = acc >= 2
    go acc [x] = acc >= 2
    go acc (x:y:xs) | acc >= 2 = True
                    | ord x == ord y - 1 = go (acc + 1) (y:xs)
                    | otherwise = go 0 (y:xs)

noContainsIOL :: String -> Bool
noContainsIOL s = not (any (`elem` "iol") s)

containsPairs :: String -> Bool
containsPairs x = go [] x
  where
    go pairs [] = length pairs >= 2
    go pairs [_] = length pairs >= 2
    go pairs (x:y:xs) | length pairs >= 2 = True
                      | isNewPair = go (x : pairs) nexts
                      | otherwise = go pairs (y:xs)
                        where
                          isNewPair = x == y && x `notElem` pairs
                          nexts = if not (null xs) && x == y && y == head xs then xs else y:xs

isValid :: String -> Bool
isValid s = containsPairs s && noContainsIOL s && containsIncreasing s

increase:: String -> String
increase [] = []
increase s = go (s, True)
  where
    go ([], True) = "a"
    go ([], False) = ""
    go (s, False) = s
    go (s', True) = let (nc, shouldCarry) = incChar (last s')
                    in go (init s', shouldCarry) ++ [nc]

incChar :: Char -> (Char, Bool)
incChar c = (chr (wrapChar nc), isOverflow nc)
  where
    nc = ord c + 1
    isOverflow c = c > ord 'z'
    wrapChar c = if isOverflow c then c `mod`ord 'z' + ord 'a' - 1 else c

skipToValid :: String -> String
skipToValid s | any (`elem` "iol") s = newS ii
              | otherwise = s
  where
    ii = minJust [elemIndex c s | c <- "iol"]
    newS Nothing = s
    newS (Just ii) = take ii s ++ [fst (incChar (s !! ii))] ++ replicate (length s - ii - 1) 'a'

run :: String -> String
run s | isValid s = s
      | any (`elem` "iol") s = run (skipToValid s)
      | otherwise = run (increase s)


main :: IO ()
main = do
  let part1Res = run "hxbxwxba"
  print $ "Part 1 -> Next password of hxbxwxba is: " ++ part1Res
  print $ "Part 2 -> Next password of hxbxwxba is: " ++ run (increase part1Res)
