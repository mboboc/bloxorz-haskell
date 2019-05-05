{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


--module Bloxorz where

--import ProblemState

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

data Cell = H | S | B | Sw | E | W
 deriving (Eq, Ord)

instance Show Cell where
 show H = charToString hardTile
 show S = charToString softTile
 show B = charToString block
 show Sw = charToString switch
 show E = charToString emptySpace
 show W = charToString winningTile

-- level e definit prin dimensiunea hartii, harta si pozitia blocului
data Level = Level Position Position (A.Array Position Cell)
    deriving (Eq, Ord)

instance Show Level where
 show (Level (x,y) posBlock arr) = "\n" ++ m where
  m = unlines [B.concat [show (arr A.! (i, j)) | i <- [0..x]] | j <- [0..y]]

makeMap :: Position->Position->A.Array Position Cell
makeMap (x,y) posBlock = a where
 a = A.array ((0,0),(x,y)) ([((i,j), E) | i <- [0..x], j <- [0..x]] ++ [(posBlock, B)])

emptyLevel :: Position -> Position -> Level
emptyLevel posColt posBlock = Level posColt posBlock (makeMap posColt posBlock)

addTile :: Char -> Position -> Level -> Level
addTile t (x,y) (Level posColt (x1, y1) arr) 
	| x == x1 && y == y1 =  Level posColt (x1, y1) arr 
	| otherwise = Level posColt (x1, y1) (arr A.// [((y,x), c)]) where c | t == 'S' = S 
  																		 | t == 'H' = H
  																		 | t == 'W' = W                     
wavefront       :: Int -> A.Array (Int,Int) Int
wavefront n     =  a  where
                   a = A.array ((1,1),(n,n))
                        ([((1,j), 1) | j <- [1..n]] ++
                         [((i,1), 1) | i <- [2..n]] ++
                         [((i,j), a A.! (i,j-1) + a A.! (i-1,j-1) + a A.! (i-1,j))
                                     | i <- [2..n], j <- [2..n]])

successors a1 b1 c1 d1 = [] ++ a ++ b ++ c ++ d where a | a1 == 1 = [1]
                                                        | otherwise = []
                                                      b | b1 == 2 = [2]
                                                        | otherwise = []
                                                      c | c1 == 3 = [3]
                                                        | otherwise = []
                                                      d | d1 == 4 = [4]
                                                        | otherwise = []

--squares = Level (1,100) [(i, i*i) | i <- [1..100]]

--instance Show Level where
	--show Level = elems Level
	--}

     