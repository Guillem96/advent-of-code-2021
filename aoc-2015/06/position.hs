module Position where
  import qualified Data.List.Split as Split
  import Data.Hashable ( Hashable(..) )

  data Position = Position2D Int Int deriving (Eq, Ord, Show)

  instance Hashable Position where
    hashWithSalt s (Position2D x1 x2) = s + hash (x1, x2)


  add::Position -> Int -> Position
  add (Position2D x y) i = Position2D (x + i) (y + i)

  posFromString :: [Char] -> Maybe Position
  posFromString s | n == 2 = Just (Position2D (head comp) (last comp))
                  | otherwise = Nothing
    where
      n = length comp
      comp = map read (Split.splitOn "," s) :: [Int]
