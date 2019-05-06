{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A
import qualified Data.List as B

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

charToString :: Char -> String
charToString c = [c]

type Position = (Int, Int)

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

data Cell = H | S | B | Sw [Position] | E | W
 deriving (Eq, Ord)

instance Show Cell where
 show H = charToString hardTile
 show S = charToString softTile
 show B = charToString block
 show (Sw a) = charToString switch
 show E = charToString emptySpace
 show W = charToString winningTile

data Level = Level Bool Position (Position, Position) (A.Array Position Cell)
    deriving (Eq, Ord)

addBlock :: (Position, Position)->A.Array Position Cell->A.Array Position Cell
addBlock (p1, p2) arr
                   | p1 == p2 = arr A.// [(p1, B)]
                   | otherwise = (arr A.// [(p1, B)]) A.// [(p2, B)]
instance Show Level where
 show (Level start (x,y) (p1,p2) arr) = "\n" ++ m where
  m = unlines [B.concat [show ((addBlock (p1,p2) arr) A.! (i, j)) | i <- [0..x]] | j <- [0..y]] ++ message where message | continueGame (Level start (x,y) (p1,p2) arr) == False && start == True = "Game Over\n" 
                                                                                                                         | p1 == p2 && (arr A.! p1) == W = "Congrats! You won!\n"
                                                                                                                         | otherwise = ""
-- creaza un array 
makeMap :: Position->A.Array Position Cell
makeMap (x,y) = a where
 a = A.array ((0,0),(x,y)) [((i,j), E) | i <- [0..x], j <- [0..y]]

emptyLevel :: Position -> Position -> Level
emptyLevel (x,y) (y1,x1) = Level False (y,x) ((x1, y1), (x1, y1)) (makeMap (y,x))

addTile :: Char -> Position -> Level -> Level
addTile t (y,x) (Level start posColt posBlock arr) = Level False posColt posBlock (arr A.// [((x,y), c)]) where c   | t == 'H' = H
                                                                                                                    | t == 'S' = S          
                                                                                                                    | t == 'W' = W
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch (y,x) lst (Level start posColt posBlock arr) = Level False posColt posBlock (arr A.// [((x,y), (Sw lst))])

-- inverseaza tuplu
inv :: Position -> Position
inv (x,y) = (y,x)

activate :: Cell -> Level -> Level
activate (Sw a) (Level start posColt posBlock arr) | (arr A.! (inv (a !! 0))) == E = Level start posColt posBlock (foldl (\x (x1, y1) -> x A.// [((y1,x1), H)]) arr a)
                                                   | otherwise = Level start posColt posBlock (foldl (\x (x1,y1) -> x A.// [((y1,x1), E)]) arr a)
activate _ lvl = lvl

move :: Directions -> Level -> Level
move South (Level start posColt ((x,y), (x1,y1)) arr)   | x == x1 && y == y1 = activate (arr A.! (x1, y1 + 2)) (activate (arr A.! (x,y + 1)) (Level True posColt ((x,y + 1), (x1, y1 + 2)) arr))
                                                        | otherwise = lvl where 
                                                            lvl | x == x1 = activate (arr A.! (x1, y1 + 1)) (Level True posColt ((x1,y1 + 1), (x1, y1 + 1)) arr)
                                                                | y == y1 = activate (arr A.! (x1, y1 + 1)) (activate (arr A.! (x,y + 1)) (Level True posColt ((x,y + 1), (x1, y1 + 1)) arr))

move North (Level start posColt ((x,y), (x1,y1)) arr)   | x == x1 && y == y1 = activate (arr A.! (x1, y1 - 2)) (activate (arr A.! (x,y - 1)) (Level True posColt ((x,y - 1), (x1, y1 - 2)) arr))
                                                        | otherwise = lvl where
                                                            lvl | x == x1 = activate (arr A.! (x, y - 1)) (Level True posColt ((x,y - 1), (x, y - 1)) arr)
                                                                | y == y1 = activate (arr A.! (x1, y1 - 1)) (activate (arr A.! (x,y - 1)) (Level True posColt ((x,y - 1), (x1, y1 - 1)) arr))

move West (Level start posColt ((x,y), (x1,y1)) arr)    | x == x1 && y == y1 = activate (arr A.! (x1 - 2, y1)) (activate (arr A.! (x - 1, y)) (Level True posColt ((x - 2,y), (x1 - 1, y1)) arr))
                                                        | otherwise = lvl where
                                                            lvl | x == x1 = activate (arr A.! (x1 - 1, y1)) (activate (arr A.! (x - 1, y)) (Level True posColt ((x - 1,y), (x1 - 1, y1)) arr))
                                                                | y == y1 = activate (arr A.! (x - 1, y)) (Level True posColt ((x - 1,y), (x - 1, y)) arr)

move East (Level start posColt ((x,y), (x1,y1)) arr)    | x == x1 && y == y1 = activate (arr A.! (x1 + 2, y1)) (activate (arr A.! (x + 1, y)) (Level True posColt ((x + 1,y), (x1 + 2, y1)) arr))
                                                        | otherwise = lvl where
                                                            lvl | x == x1 = activate (arr A.! (x1 + 1, y1)) (activate (arr A.! (x + 1, y)) (Level True posColt ((x + 1,y), (x1 + 1, y1)) arr))
                                                                | y == y1 = activate (arr A.! (x1 + 1, y1)) (Level True posColt ((x1 + 1, y1), (x1 + 1, y1)) arr)

-- returneaza True daca jocul nu este pierdut
continueGame :: Level -> Bool
continueGame (Level start posColt (p1, p2) arr) | p1 == p2 && (arr A.! p1) == S = False
                                                | (arr A.! p1) == E || (arr A.! p2) == E = False
                                                | otherwise = True
instance ProblemState Level Directions where
    successors lvl = [] ++ a ++ b ++ c ++ d where a | continueGame (move North lvl) == True = [(North, (move North lvl))]
                                                    | otherwise = []
                                                  b | continueGame (move South lvl) == True = [(South, (move South lvl))]
                                                    | otherwise = []
                                                  c | continueGame (move East lvl) == True = [(East, (move East lvl))]
                                                    | otherwise = []
                                                  d | continueGame (move West lvl) == True = [(West, (move West lvl))]
                                                    | otherwise = []
    isGoal (Level start posColt (p1, p2) arr) | p1 == p2 && (arr A.! p1) == W = True
                                              | otherwise = False

    -- Doar petru BONUS
    -- heuristic = undefined