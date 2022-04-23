import qualified Data.Map   as Map
import           JSONParser (JValue (JArray, JNumber, JObject, JString),
                             parseJSON)


nums :: JValue -> [Int]
nums = go []
  where
    go acc (JNumber i _ _) = acc ++ [fromIntegral i]
    go acc (JArray xs)     = acc ++ concatMap nums xs
    go acc (JObject xs)    = acc ++ concatMap (nums . snd) xs
    go acc _               = acc

nums' :: JValue -> [Int]
nums' = go []
  where
    go acc (JNumber i _ _) = acc ++ [fromIntegral i]
    go acc (JArray xs)     = acc ++ concatMap nums' xs
    go acc j@(JObject xs) | j `jValueContains` "red" = acc
                          | otherwise = acc ++ concatMap (nums' . snd) xs
    go acc _ = acc

jValueContains :: JValue -> String -> Bool
(JString s)  `jValueContains` x = s == x
(JObject xs) `jValueContains` x = or [o == x | JString o <- map snd xs]
_ `jValueContains` _            = False

main :: IO ()
main = do
  input <- readFile "input.json"
  let mm = parseJSON input
  print $ fmap (sum . nums) mm
  print $ fmap (sum . nums') mm
