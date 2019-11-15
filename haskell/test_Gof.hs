module Test_Gof where

import qualified Data.Set as Set
import Test.HUnit
import Gof

test_hello = TestCase (assertEqual "hello" (2) hello)

test_are_neighbors1 = TestCase (assertEqual "should be neighbors 1,1 and 1,0" (True) (areNeighbors (1, 1) (1, 0)))
test_are_neighbors2 = TestCase (assertEqual "should be neighbors 1,1 and 1,2" (True) (areNeighbors (1, 1) (1, 2)))
test_are_neighbors3 = TestCase (assertEqual "should be neighbors 1,1 and 0,1" (True) (areNeighbors (1, 1) (0, 1)))
test_are_neighbors4 = TestCase (assertEqual "should be neighbors 1,1 and 2,1" (True) (areNeighbors (1, 1) (2, 1)))
test_are_neighbors5 = TestCase (assertEqual "should be neighbors 1,1 and 0,0" (True) (areNeighbors (1, 1) (0, 0)))
test_are_neighbors6 = TestCase (assertEqual "should be neighbors 1,1 and 2,2" (True) (areNeighbors (1, 1) (2, 2)))
test_are_neighbors7 = TestCase (assertEqual "should be neighbors 1,1 and 0,2" (True) (areNeighbors (1, 1) (0, 2)))
test_are_neighbors8 = TestCase (assertEqual "should be neighbors 1,1 and 2,0" (True) (areNeighbors (1, 1) (2, 0)))

test_print = TestCase (assertEqual "should print game" ("Generation: 1") (print_game (Set.fromList [], 1)))
test_init = TestCase (assertEqual "should init game" (Set.fromList [(1, 1)], 1) (init_game [(1, 1)]))

test_count_n = TestCase (assertEqual "should have 2 neighbors" (2) (countN (Set.fromList [(1, 1), (2, 2), (3, 3)]) (1, 2)))
test_count_n2 = TestCase (assertEqual "should have 2 neighbors" (2) (countN (Set.fromList [(1, 1), (1, 2), (2, 2)]) (1, 2)))
test_count_n3 = TestCase (assertEqual "should have 2 neighbors" (2) (countN (Set.fromList [(1, 1), (1, 2), (2, 2)]) (2, 2)))

test_survivors1 = TestCase (assertEqual "game with one cell should have zero survivors" (Set.empty) (survivors (init_game [(1, 1)])))
test_survivors2 = TestCase (assertEqual "game with two cells should have zero survivors" (Set.empty) (survivors (init_game [(1, 1), (1, 2)])))
test_survivors3 = TestCase (assertEqual "game with three neighbor cells should have three survivors" (Set.fromList [(1, 1), (1, 2), (2, 2)]) (survivors (init_game [(1, 1), (1, 2), (2, 2)])))


test_neighbors = TestCase (assertEqual "neighbors of (1,1) are (0,1),(0,2),(1,0),(1,2),(0,0),(2,2)" (Set.fromList [(0,0),(0,1),(1,0),(1,2),(2,1),(2,2)]) (neighbors (1, 1)))

test_newborn = TestCase (assertEqual "newbor of (0,0),(1,0),(0,1) is (1,1)" (Set.fromList [(1, 1)]) (newborn (init_game [(0, 0), (1, 0), (0, 1)])))

test_next_generation1 = TestCase (assertEqual "next generation of (1,1),(0,2) is empty" ((Set.empty), 2) (next_generation (init_game [(1, 1), (0, 2)])))
test_next_generation2 = TestCase (assertEqual "next generation of (0,1),(0,2),(1,0),(1,1)" ((Set.fromList [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]), 2) (next_generation (init_game [(0, 1), (0, 2), (1, 0), (1, 1)])))
test_next_generation3 = TestCase (assertEqual "next generation of full sqare" ((Set.fromList [(-1,1),(0,0),(0,2),(1,-1),(2,0),(2,2),(3,1)]), 2) (next_generation (init_game [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 1), (2, 0), (2, 1), (2, 2)])))
test_next_generation4 = TestCase (assertEqual "next generation of (1,0),(0,1),(0,2) is (1,0),(1,1),(0,1),(0,2)" ((Set.fromList [(0,1),(1,1)]), 2) (next_generation (init_game [(0, 1), (0, 2), (1, 0)])))

tests = TestList 
    [ (test_are_neighbors1)
    , test_are_neighbors2
    , test_are_neighbors3
    , test_are_neighbors4
    , test_are_neighbors5
    , test_are_neighbors6
    , test_are_neighbors7
    , test_are_neighbors8
    , test_print
    , test_init
    , test_count_n
    , test_count_n2
    , test_count_n3
    , test_survivors1
    , test_survivors2
    , test_survivors3
    , test_neighbors
    , test_newborn
    , test_next_generation1
    , test_next_generation2
    , test_next_generation3
    , test_next_generation4 ]
main = do runTestTT tests
