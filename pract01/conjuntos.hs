module Conjuntos where
import Data.List
--Definicion de Conjunto
data Set a = ESet | CSet a (Set a) deriving (Read,Show)

--listToSet: Funcion que recibe una lista y devuelve un Set
listToSet :: Eq a => [a] -> Set a
listToSet xs = listToSet' (nub xs) where
                listToSet' [] = ESet
                listToSet' (x:xs) = CSet x (listToSet' xs)

--SetToList: Función que recibe un Set a y devuelve una lista de 
setToList :: Set a -> [a]
setToList ESet = []
setToList (CSet x s) = x:(setToList s)

--isEmpty: Función que dado un Set a devuelve True si es el conjunto vacío (ESet)
isEmpty::Set a -> Bool
isEmpty ESet = True
isEmpty _ = False

--inSet: Función que dado un valor a y un Set a devuelve verdadero
--si el elemento esta en el conjunto
inSet::Eq a => a->Set a-> Bool
inSet x ESet = False
inSet x (CSet a s) = if a==x then True else inSet x s 

--addToSet: Funcion que agrega un elemento a un conjunto
addToSet:: Eq a => a -> Set a -> Set a
addToSet a ESet = CSet a ESet
addToSet a set@(CSet x s) = if a == x then set 
                        else CSet x (addToSet a s)               

--unionS: Funcion que dados dos conjuntos, crea un nuevo conjunto en base a los dos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS s1 ESet = s1
unionS s1 (CSet x s2) = unionS (addToSet x s1) s2 
