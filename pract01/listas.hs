module Listas where

--3.Listas
--atN: dada una lista y un número n devuelve el n-esimo elemeno de la lista
atN :: [a] -> Int -> a
atN [] _ = error "La lista esta vacia"
atN x 0 = head x
atN (x:xs) n = atN xs (n-1)

--ktN: devulve una lista de los primeros k elementos, donde k es el elemento
--menor de la lista
ktN :: [Int] -> [Int]
ktN xs = take (minimum xs) xs

--makeTuplas
makeTuplas :: [a] -> [(a,a)]
makeTuplas [] = [] 
makeTuplas [x] = [(x,x)]
makeTuplas [x,y] = [(x,y)]
makeTuplas (x:xs) = [(x,last xs)] ++ makeTuplas (init xs)