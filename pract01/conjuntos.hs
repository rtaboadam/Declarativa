module Conjuntos where
import Data.List

--Definicion de Conjunto
data Set a = ESet | CSet a (Set a) deriving (Read,Eq)

--Imprimir los Set en consola
instance Show a => Show (Set a) where
    show set = case set of
                ESet -> "{}"
                s -> "{" ++ aux s where
                    aux ESet = ""
                    aux (CSet a ESet) = show a ++ "}"
                    aux (CSet a as) = show a ++ "," ++ aux as                              


--listToSet: Funcion que recibe una lista y devuelve un Set
--Para cumplir la definición de conjunto le quitamos los 
--elementos repetidos a las listas.
listToSet :: Eq a => [a] -> Set a
listToSet xs = listToSet' (nub xs) where
                listToSet' [] = ESet
                listToSet' (x:xs) = CSet x (listToSet' xs)

--SetToList: Función que recibe un Set a y devuelve una lista 
--de elementos de a 
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

--intersectionS: Funcion que intersecta dos conjuntos
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS ESet _  = ESet
intersectionS s1@(CSet x xs) s2 = if x `inSet` s2 then CSet x (intersectionS xs s2)
                                  else intersectionS xs s2

--diffS: Función que regresa el conjunto diferencia de dos Set
diffS ::Eq a => Set a ->Set a ->Set a
diffS ESet _ = ESet
diffS (CSet a xs) s2 = if not (a `inSet` s2) then CSet a (xs `diffS` s2)
                       else xs `diffS` s2

--subSet: Función que dice si un conjunto es subconjunto de otro
subSet:: Eq a => Set a -> Set a -> Bool
subSet ESet _ = True
subSet (CSet a xs) s2 = if a `inSet` s2 then subSet xs s2 else False

--subSets: Función que da el conjunto potencia de un conjunto.
subSets:: Eq a => Set a -> Set (Set a)
subSets ESet = CSet ESet ESet
subSets (CSet s ss) = let powS = subSets ss in
                      powS `unionS` auxSubSets s powS

--Auxiliar para la función principal subSets
auxSubSets:: Eq a => a -> Set (Set a) -> Set (Set a)
auxSubSets x ESet = ESet
auxSubSets x (CSet s ss) = CSet (x `addToSet` s) (auxSubSets x ss)

--removeElem: Función que quita un elemento de un conjunto
removeElem:: Eq a => a-> Set a -> Set a
removeElem _ ESet = ESet
removeElem a (CSet x xs) = if a == x then xs else (CSet x (removeElem a xs)) 

--equalsS: Funcion que dictamina si dos conjuntos son iguales
equalsS :: Eq a => Set a -> Set a -> Bool
equalsS ESet ESet = True
equalsS ESet _ =False
equalsS _ ESet = False
equalsS (CSet x xs) s1 = if x `inSet` s1 then equalsS xs (removeElem x s1) 
                         else False
