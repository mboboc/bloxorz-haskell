{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A
import qualified Data.List as B

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

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

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = H | S | B | Sw | E | W
 deriving (Eq, Ord)

instance Show Cell where
 show H = charToString hardTile
 show S = charToString softTile
 show B = charToString block
 show Sw = charToString switch
 show E = charToString emptySpace
 show W = charToString winningTile

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level Position  (Position, Position) (A.Array Position Cell) (A.Array Position [Position, Cell])
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

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

--addBlock :: Level -> Level
--addBlock (Level posColt (y,x) arr arrS) = Level posColt (y,x) (arr A.// [((x,y), B)]) arrS

--addBlock :: Position->Position->A.Array Position Cell->A.Array Position [Position]
--addBlock posColt (x1,y1) arr arrS = Level posColt (x1, y1) arr 

addBlock :: (Position, Position)->A.Array Position Cell->A.Array Position Cell
addBlock posBlock arr
                   | fst (snd posBlock) == -1 && snd (snd posBlock) == -1 = arr A.// [(fst posBlock, B)]
                   | otherwise = (arr A.// [((fst posBlock), B)]) A.// [((snd posBlock), B)]

instance Show Level where
 show (Level (x, y) posBlock arr arrS) = "\n" ++ m where
  m = unlines [B.concat [show ((addBlock posBlock arr) A.! (i, j)) | i <- [0..x]] | j <- [0..y]]

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

makeMap :: Position->A.Array Position Cell
makeMap (x,y) = a where
 a = A.array ((0,0),(x,y)) [((i,j), E) | i <- [0..x], j <- [0..y]]

makeArrS :: Position->A.Array Position [Position]
makeArrS (x,y) = a where
 a = A.array ((0,0), (x,y)) [((i,j), []) | i <- [0..x], j <- [0..y]]

emptyLevel :: Position -> Position -> Level
emptyLevel (x,y) (x1, y1) = Level (y,x) ((y1, x1), (-1, -1)) (makeMap (y,x)) (makeArrS (y,x))

{-
    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile t (y,x) (Level posColt posBlock arr arrS) = Level posColt posBlock (arr A.// [((x,y), c)]) arrS where c | t == 'S' = S 
                                                                                                                | t == 'H' = H
                                                                                                                | t == 'W' = W
{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [(Position, Cell)] -> Level -> Level
addSwitch (y,x) lst (Level posColt (x1, y1) arr arrS) = Level posColt (x1, y1) (arr A.// [((x,y), Sw)]) (arrS A.// [(((x,y), E), lst)])

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Position -> Level -> Level
activate pos (Level posColt posBlock arr arrS) = 

	Level posColt posBlock (foldl (\acc tilePos -> addTile tileType tilePos acc) (arrS A.! pos) arr) arrS 
	where tileType
			| arr A
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
