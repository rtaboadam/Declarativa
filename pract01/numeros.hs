module Numeros where

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
getaveragethree:: Float -> Float -> Float -> [Float]
getaveragethree x y z = [w | w <- [x,y,z] ,(((x+y+z)/3) < w)]

