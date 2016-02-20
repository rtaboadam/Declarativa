module ListasComprension where

--4.Listas por comprensión
--[0,1,3,7,15,31,...]
fstlist::Integral a => [a]
fstlist = [2^i - 1 | i <- [0..]]
--[(3,4),(7,8),(11,12)..]
sndlist = [(a-1,a) | a <- [4,8..]]
--[58,65,72,...]
trdlist = [a | a<-[58,65..]]
--ternas pitagoricas
ternaPitagoricas = [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..100], c**2 == a**2 + b**2]

--Funciones de orden superior
--Fibonacci
tribo::Int -> [Int]
tribo n = [tribo' x| x <-[0..n]] where
        tribo' 0 = 1
        tribo' 1 = 1
        tribo' 2 = 1
        tribo' n = tribo'(n-1) + tribo' (n-2) + tribo' (n-3)
