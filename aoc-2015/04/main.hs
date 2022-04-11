import Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.MD5

numZeros = 6 -- Change this to 5 for part 1

leadingZeros = replicate' numZeros "0"

replicate' :: Int -> String -> String
replicate' 0 _ = ""
replicate' n s = s ++ replicate' (n - 1) s

isHashValid :: MD5Digest -> Bool
isHashValid hash = Prelude.take numZeros parsedHash == leadingZeros
  where
    parsedHash = show hash

hashSecret = md5 . LBS.fromStrict . C8.pack

mine secret i = if isHashValid newSecret then i else mine secret (i + 1)
  where
    newSecret = hashSecret $ secret ++ show i

main = do
  print $ mine "ckczppom" 0