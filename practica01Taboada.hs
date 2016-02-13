--Ricardo Taboada Magallanes
--
module SeccionFoo where

--1
--Las operaciones posibles entre dos valores booleanos son 16.
--A continuación se definiran las 16 operaciones.

--La op0001 es mejor conocida como AND
op0001 = \x -> \y -> x && y
--
op0010 = \x -> \y -> x && not y
--
op0011 = \x -> \y -> op0001 x y `or'` op0010 x y
--
or' = \x -> \y -> not $ not x && not y
