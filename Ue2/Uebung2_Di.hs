module Uebung2_Di where 
import Prelude hiding (words)

--Simple Examples on type constructors

data Day = Morning|Noon|Evening
data Something = This | That
{-Type-constructor for Something given, no data constructor named Something
try:
*Uebung2_Di> let x = This   ...works
*Uebung2_Di> :t x
x::Something
*Uebung2_Di> let x = Something           ...doesn't work
-}

--Something as type- and dataconstructor -> separate name spaces
-- data Something = Something 

exmplFunction :: Something -> Bool
exmplFunction This = True
exmplFunction That = False

pack::[Char]->[[Char]]
pack [] = []
pack (x:xs) = aux [x] xs
  where aux sorted [] = [sorted]
        aux (x:sorted) (y:ys)
         | x==y = aux (x:y:sorted) ys
         | otherwise = (x:sorted) : aux [y] ys 

encode, encode' ::[Char] -> [(Int, Char)]
encode [] = []
encode xs = aux (pack xs) 
 where aux []  = []
       aux (xs:xss) = (length xs, head xs) : aux xss

encode' lss = [(length ls, head ls)| ls<- pack lss] 

decode::[(Int, Char)] -> [Char]
decode [] = []
decode ((count, letter) : tuples) = times count letter ++ decode tuples
     where times count letter
             |count==0 = []
             |otherwise = letter : times (count-1) letter



rotate::[Int]-> Int-> [Int]
rotate [] _ = []
rotate [x]_ = [x]
rotate xs 0 = xs
rotate list@(x:xs) count
   | count > 0 = rotate (xs ++[x]) (count -1)
   | count < 0 = rotate (last list : init list) (count+1)

words :: String -> [String]
--is equal to:  words::[Char] -> [[Char]]
words [] = []
words (char: chars) = helper [char] chars
  where helper (c:cs) [] = [(c:cs)]
        helper (c:cs) (ch:chs)
         | ch==' ' = (c:cs) : helper [ch] chs
         | otherwise = helper ((c:cs)++[ch]) chs








