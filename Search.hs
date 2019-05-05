{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;s
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node { state :: s
                     , action :: Maybe a
                     , parent :: Maybe (Node s a)
                     , children :: ([(a, s)])
                     } deriving (Show)

instance Eq s => Eq (Node s a) where
 node1 == node2 = ((state node1) == (state node2))

instance Ord s => Ord (Node s a) where
 node1 <= node2 = ((state node1) <= (state node2))

nodeState :: Node s a -> s
nodeState node = state node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace s = (Node {state = s, action = Nothing, parent = Nothing, children = (successors s)})

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

myflip :: (a1 -> a2 -> a3 -> r) -> a3 -> a2 -> a1 -> r
myflip f x1 x2 x3 = f x3 x2 x1
 

limitedTailRecursive :: (ProblemState s a, Ord s) => Ord s => Node s a -> Int -> (S.Set (Node s a)) -> [Node s a]
limitedTailRecursive node (-1) seet = []
limitedTailRecursive node h seet | (S.member node seet) == True = [] ++ (concat (map ((myflip limitedTailRecursive) seet (h-1)) (helper node)))
                                 | otherwise = [node] ++ (concat (map ((myflip limitedTailRecursive) (S.insert node seet) (h-1)) (helper node)))

helper :: (ProblemState s a, Ord s) => Node s a -> [Node s a]
helper node = (map (\(x1, y1) -> (Node {state = y1, action = Just x1, parent = (Just node), children = (successors y1)})) (successors (nodeState node)))

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs node (-1) = []
limitedDfs node h = (limitedTailRecursive node h S.empty)

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening = undefined

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath = undefined

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve = undefined

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))