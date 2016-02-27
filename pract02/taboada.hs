--module foo where
import Data.List

--elimDup: Borra los elementos repetidos que son adyacentes.
elimDup ::Eq a => [a] -> [a]
elimDup xs = foldr f [] xs

f :: Eq a => a -> [a] -> [a]
f x [] = [x]
f x l@(y:ys) = if x == y then l else (x:l)

--Funcion que nos devuelve las sublistas de una lista.
sublists [] = []
sublists (x:xs) = tail (inits (x:xs)) ++ (sublists xs)

maxsumas::[Int] -> Int
maxsumas [] = 0
maxsumas (xs) = maximum (map sum (sublists xs))
