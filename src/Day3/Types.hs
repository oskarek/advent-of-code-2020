module Day3.Types (MapPos (..), TreeMap, Slope) where

data MapPos = Tree | Free deriving (Eq)
type TreeMap = [[MapPos]]
type Slope = (Int, Int)