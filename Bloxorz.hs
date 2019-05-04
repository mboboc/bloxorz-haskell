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

data Cell = H | S | B | Sw | E | W
 deriving (Eq, Ord)

instance Show Cell where
 show H = charToString hardTile
 show S = charToString softTile
 show B = charToString block
 show Sw = charToString switch
 show E = charToString emptySpace
 show W = charToString winningTile

data Level = Level Position (Position, Position) (A.Array Position Cell) (A.Array Position [Position])
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

--addBlock :: Level -> Level
--addBlock (Level posColt (y,x) arr arrS) = Level posColt (y,x) (arr A.// [((x,y), B)]) arrS

--addBlock :: Position->Position->A.Array Position Cell->A.Array Position [Position]
--addBlock posColt (x1,y1) arr arrS = Level posColt (x1, y1) arr 

addBlock :: (Position, Position)->A.Array Position Cell->A.Array Position Cell
addBlock (p1, p2) arr
                   | p1 == p2 = arr A.// [(p1, B)]
                   | otherwise = (arr A.// [(p1, B)]) A.// [(p2, B)]
instance Show Level where
 show (Level (x,y) posBlock arr arrS) = "\n" ++ m where
  m = unlines [B.concat [show ((addBlock posBlock arr) A.! (i, j)) | i <- [0..x]] | j <- [0..y]]

makeMap :: Position->A.Array Position Cell
makeMap (x,y) = a where
 a = A.array ((0,0),(x,y)) [((i,j), E) | i <- [0..x], j <- [0..y]]

makeArrS :: Position->A.Array Position [Position]
makeArrS (x,y) = a where
 a = A.array ((0,0), (x,y)) [((i,j), []) | i <- [0..x], j <- [0..y]]

emptyLevel :: Position -> Position -> Level
emptyLevel (x,y) (y1,x1) = Level (y,x) ((x1, y1), (x1, y1)) (makeMap (y,x)) (makeArrS (y,x))

addTile :: Char -> Position -> Level -> Level
addTile t (y,x) (Level posColt posBlock arr arrS) = Level posColt posBlock (arr A.// [((x,y), c)]) arrS where c | t == 'S' = S 
                                                                                                                | t == 'H' = H
                                                                                                                | t == 'W' = W
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch (y,x) lst (Level posColt posBlock arr arrS) = Level posColt posBlock (arr A.// [((x,y), Sw)]) (arrS A.// [((x,y), lst)])

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***
    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Cell -> Level -> Level
activate = undefined

{-
    *** TODO ***
    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move = undefined

{-
    *** TODO ***
    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame = undefined

{-
    *** TODO ***
    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined