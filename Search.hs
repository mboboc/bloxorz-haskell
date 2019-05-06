{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe

data Node s a = Node { state :: s
                     , action :: Maybe a
                     , parent :: Maybe (Node s a)
                     , children :: ([(a, s)])
                     } deriving (Show)

--instance Eq s => Eq (Node s a) where
-- node1 == node2 = ((state node1) == (state node2))

--instance Ord s => Ord (Node s a) where
-- node1 <= node2 = ((state node1) <= (state node2))

nodeState :: Node s a -> s
nodeState node = state node

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace s = (Node {state = s, action = Nothing, parent = Nothing, children = (successors s)})

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined

-- flips arguments of a function
myflip :: (a1 -> a2 -> a3 -> r) -> a3 -> a2 -> a1 -> r
myflip f x1 x2 x3 = f x3 x2 x1

-- check if element is inside a list
isElem :: (Eq s) => [(Node s a)] -> (Node s a) -> Bool
isElem [] _ = False
isElem (x:xs) node | (state node) == (state x) = True
                   | otherwise = (isElem xs node)

-- sterge dublicatele dintr-o lista
deleteDuplicates :: (Eq s) => [Node s a] -> [Node s a] -> [Node s a]
deleteDuplicates [] filt = filt
deleteDuplicates (x:original) filt | (isElem filt x) = (deleteDuplicates original filt)
                                   | otherwise = (deleteDuplicates original (filt ++ [x]))
 
 -- am trasformat recursivitatea pe stiva in recursivitate pe coada
limitedTailRecursive :: (ProblemState s a, Ord s) => Node s a -> Int -> [(Node s a)] -> [(Node s a)]
limitedTailRecursive _ (-1) _ = []
limitedTailRecursive node h seet = [node] ++ (concat (map ((myflip limitedTailRecursive) ([node] ++ seet) (h-1)) (helper node)))

-- creeaza o lista de noduri din copii unei stari
helper :: (ProblemState s a, Ord s) => Node s a -> [Node s a]
helper node = (map (\(x1, y1) -> (Node {state = y1, action = Just x1, parent = (Just node), children = (successors y1)})) (successors (nodeState node)))

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs node h = deleteDuplicates (limitedTailRecursive node h []) []

-- primeste o lista si returneaza o alta lista cu tupluri (index, element); indexul incepe de la 1
myAssoc :: [Node s a] -> [(Int, Node s a)]
myAssoc list = zip [1..] list

-- filtreaza lista, raman doar perechile cu stare castigatoare, functia returneaza starea primului element
myFiler :: [(Bool, (Node s a, Int))] -> (Node s a, Int)
myFiler lst = snd (head (filter (\(x, (y,z)) -> x == True) lst))

iterativeDeepening :: (ProblemState s a, Ord s) => Node s a -> (Node s a, Int)
iterativeDeepening node = myFiler (map (\(x1 , y1) -> ((isGoal (nodeState y1)), (y1, x1))) (myAssoc (concat (map (\x2 -> (limitedDfs node x2)) [1..1000]))))

-- functie helper care parcurge parintii
extractHelper :: Eq a => Maybe (Node s a) -> [(a, s)]
extractHelper node |(action (fromJust node)) == Nothing = []
                   | otherwise = [((fromJust (action (fromJust node))), (state (fromJust node)))] ++ (extractHelper (parent (fromJust node)))

extractPath :: Eq a => Node s a -> [(a, s)]
extractPath node = reverse ([((fromJust (action node)), (state node))] ++ (extractHelper (parent node)))

solve :: (ProblemState s a, Ord s) => (Eq a) => s -> Bool -> [(a, s)]
solve = undefined -- = extractPath (fst (iterativeDeepening (createStateSpace s)))

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))