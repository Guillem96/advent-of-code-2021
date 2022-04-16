module CoordCompression
  ( CoordinateCompression2D,
    fromCoords,
    compress,
    decompress,
  )
where

import qualified Data.List as List
import qualified Data.Map as Map
import Position

data CoordinateCompression2D = CoordinateCompression2D
  { xmap :: Map.Map Int Int,
    ymap :: Map.Map Int Int,
    xs :: [Int],
    ys :: [Int]
  }
  deriving (Show)

fromCoords :: [Position] -> CoordinateCompression2D
fromCoords ps = CoordinateCompression2D (Map.fromList isxs) (Map.fromList isys) sxs sys
  where
    sxs = List.sort [x | (Position2D x _) <- ps]
    sys = List.sort [y | (Position2D _ y) <- ps]
    isxs = zip sxs [0 ..]
    isys = zip sys [0 ..]

compress :: CoordinateCompression2D -> Position -> Position
compress (CoordinateCompression2D xmap ymap _ _) (Position2D x y) = Position2D newx newy
  where
    newx = Map.findWithDefault (-1) x xmap
    newy = Map.findWithDefault (-1) y ymap

decompress :: CoordinateCompression2D -> Position -> Position
decompress (CoordinateCompression2D _ _ xs ys) (Position2D x y) = Position2D newx newy
  where
    newx = xs !! x
    newy = ys !! y