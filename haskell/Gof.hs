module Gof where

import qualified Data.Set as Set
--import Data.Set (Set, fromList)
--import qualified Data.Set as Set

hello :: Int
hello = 2

type Address = (Int, Int)
type Cells = Set.Set Address
type Generation = Int

type Game = (Cells, Generation)

areNeighbors :: Address -> Address -> Bool
areNeighbors (x1, y1) (x2, y2) = 
    (x1    , y1 - 1) == (x2, y2) || 
    (x1    , y1 + 1) == (x2, y2) ||
    (x1 + 1, y1    ) == (x2, y2) ||
    (x1 - 1, y1    ) == (x2, y2) ||
    (x1 - 1, y1 - 1) == (x2, y2) ||
    (x1 + 1, y1 + 1) == (x2, y2) ||
    (x1 + 1, y1 - 1) == (x2, y2) ||
    (x1 - 1, y1 + 1) == (x2, y2)


init_game :: [Address] -> Game
init_game cells = (Set.fromList cells, 1)

print_game :: Game -> String
print_game (cells, generation) = "Generation: " ++ (show generation) ++ "\n" ++ (print_cells cells)

print_cells :: Cells -> String
print_cells cells = Set.showTree cells

next_generation :: Game -> Game
next_generation game = ((Set.union (survivors game) (newborn game)), (snd game) + 1)

countN :: Cells -> Address -> Int
countN cells address = sum (map (\x -> fromEnum (areNeighbors x address)) (Set.toList cells))

survivors :: Game -> Cells
survivors (cells, generation) = Set.filter (\x -> (elem (countN cells x) [2, 3])) cells

neighbors :: Address -> Cells
neighbors (x, y) = Set.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x - 1, y - 1), (x + 1, y + 1)]

newborn :: Game -> Cells
newborn (cells, generation) = Set.fromList (filter (\x -> (countN cells x) == 3) ((Set.toList (Set.map (\x -> neighbors x) cells)) >>= \x -> Set.toList x))


--main :: IO()
--main = putStr (show (areNeighbors (1, 3) (2, 3)))
--main = putStr (show (sa (1, 3)))