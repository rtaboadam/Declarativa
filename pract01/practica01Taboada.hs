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


--Pruebas
--Las posibles combinaciones de dos valores booleanos
valores = [(False,False),(False,True),(True,False),(True,True)]
--Funcion para obtener todos los posibles valores de un operador
checaOp = checaOp' valores
--Función auxiliar de checaOp
checaOp' :: [(Bool,Bool)] -> (Bool -> Bool -> Bool) -> [Bool]
checaOp' [] _ = []
checaOp' ((a,b):xs) f = (f a b):(checaOp' xs f)