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
print_cells cells = map star [((Set.member (x, y) cells), y == (snd (top_right cells))) | x <- [(fst (bottom_left cells))..(fst (top_right cells))], y <- [(snd (bottom_left cells))..(snd (top_right cells))]] >>= \x -> x

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

bottom_left :: Cells -> Address
bottom_left cells = ((foldr1 min (Set.map fst cells)), (foldr1 min (Set.map snd cells)))

top_right :: Cells -> Address
top_right cells = ((foldr1 max (Set.map snd cells)), (foldr1 max (Set.map snd cells)))

minc :: Address -> Int
minc (x, y) = min x y

maxc :: Address -> Int
maxc (x, y) = max x y

--line :: Cells -> Int -> String
--line cells row = map star [(Set.member (row, x) cells) | x <- [(minc (bottom_left cells))..(maxc (top_right cells))]]

star :: (Bool, Bool) -> String
star (True,False) = ['*']
star (False,False) = [' ']
star (True,True) = ['*', '\n']
star (False,True) = [' ', '\n']

play_game :: Game -> Int -> String
play_game game 0 = print_game game
play_game game n = (print_game game) ++ (play_game (next_generation game) (n - 1))

main :: IO()
main = putStr (play_game (init_game [(0, 0), (0, 1), (0, 2), (1, 1), (1, 2), (1, 3)]) 3)
