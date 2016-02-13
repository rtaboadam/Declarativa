--Ricardo Taboada Magallanes
--
module SeccionFoo where

--1.- Da los 16 operadores de valores booleanos
op0000 = \x -> \y -> False
--Mejor conocido como AND
op0001 = \x -> \y -> x && y
--
op0010 = \x -> \y -> x && not y
--
op0011 = \x -> \y -> op0001 x y `or'` op0010 x y
--
op0100 = \x -> \y -> not x && y
--
op0101 = \x -> \y -> op0001 x y `or'` op0100 x y
--Mejor conocido como XOR   
op0110 = \x -> \y -> op0010 x y `or'` op0100 x y
--
op0111 = \x -> \y -> op0110 x y `or'` op0001 x y
--
op1000 = \x -> \y -> not x && not y
--Mejor conocido como Equivalencia
op1001 = \x -> \y -> op0001 x y `or'` op1000 x y
--
op1010 = \x -> \y -> op0010 x y `or'` op1000 x y
--
op1011 = \x -> \y -> op1010 x y `or'` op0001 x y
--
op1100 = \x -> \y -> op1000 x y `or'` op0100 x y
--Mejor conocido como Implicacion
op1101 = \x -> \y -> op1100 x y `or'` op0001 x y
--Mejor conocido como OR
op1110 = \x -> \y -> not $ op0001 x y
--
op1111 = \x -> \y -> True
--
or' = \x -> \y -> not $ not x && not y

--2.Numeros
--Define la función max' que devuelve el número mayor de dos numeros
max':: Ord a => a -> a -> a
max' n m = if n > m then n else m 
--maxthree: Función que dados tres numeros devuelve el mayor
maxthree:: Ord a => a -> a -> a -> a
maxthree x y z = let w = max' x y in
                    if w > z then w else z

--media: Función que da el promedio de tres numeros
media ::Float -> Float -> Float -> Float
media x y z = (x + y + z)/3 

--getaveragethree: recibe tres enteros y devuelve el numero que es mayor
getaveragethree::Float->Float-> Float->Float
getaveragethree x y z = let w = media x y z in
                        if x > w then x else
                            if y > w then y else
                                if z > w then z else error "Raro"

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

--4.Listas por comprensión
ternaPitagoricas = [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..100], c**2 == a**2 + b**2]
--Pruebas
--Las posibles combinaciones de dos valores booleanos
valores = [(False,False),(False,True),(True,False),(True,True)]
--Funcion para obtener todos los posibles valores de un operador
checaOp = checaOp' valores
--Función auxiliar de checaOp
checaOp' :: [(Bool,Bool)] -> (Bool -> Bool -> Bool) -> [Bool]
checaOp' [] _ = []
checaOp' ((a,b):xs) f = (f a b):(checaOp' xs f)
