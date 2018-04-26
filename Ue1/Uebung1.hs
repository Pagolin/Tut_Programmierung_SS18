-- Autor:  Lisza Zeidler <lisza.zeidler@tu-dresden.de>
-- Benutzung:  $ ghci Uebung1.hs

module Uebung1 where

import Prelude hiding (rem)

fac::Int->Int
fac 0 = 1
fac n = n* fac(n-1)

fac' :: Int-> Int
fac' x = product[1..x]

sumFacs, sumFacs'::Int-> Int->Int
sumFacs n m  
       | m<n = 0
       | otherwise = fac n + sumFacs (n+1) m

sumFacs' n m = sum[fac x| x<- [n..m]]

fib::Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1)+ fib(n-2)

fib'::Int->Int
fib' n 
    | n < 2 = 1
    | otherwise =  fib'(n-1)+ fib'(n-2)

prod::[Int]-> Int
prod [] = 1
prod (x:xs) = x * prod xs

prod1:: [Int] -> Int
prod1 xs | xs == [] = 1
         | otherwise = head xs * prod (tail xs)

---------------------------------------------------------------------------------------------------
rev::[Int]->[Int]
rev [] = []
rev (x:xs) = rev xs ++[x]

rem:: Int -> [Int]-> [Int]
rem _ [] = []
rem n (x:xs) 
         | n==x = rem n xs
         | otherwise = x: rem n xs


isOrd::[Int]-> Bool
isOrd []  = True
isOrd [x] = True
isOrd (x:y:xs) 
          | x > y = False
          | otherwise = isOrd (y:xs)
 
isOrd'::[Int]-> Bool
isOrd' []  = True
isOrd' [x] = True
isOrd' (x:y:xs)= (x<=y) && isOrd' (y:xs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)| x<=y = x : merge xs (y:ys)
                   | otherwise = y : merge (x:xs) ys

--With if..then...else.. Avoid that!
merge'::[Int]->[Int]->[Int]
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs)(y:ys) = if (x<=y) then x: merge xs (y:ys) else y: merge (x:xs) ys


--Infinite fibonacci list
fib::Int -> Int
fib x | x < 2 = 1
      | otherwise = fib (x-1) + fib (x-2)

fibList::[Int]
fibList = [fib x| x<-[0,1..]]




