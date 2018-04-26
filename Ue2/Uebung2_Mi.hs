module Uebung2_Mi where
import Prelude hiding (unwords, words)


---Simple Examples on type constructors

data Day = Morning|Noon|Evening
data Something = This | That
{-Type-constructor for Something given, no data constructor named Something
try:
*Uebung2_Mi> let x = This   ...works
*Uebung2_Mi> :t x
x::Something
*Uebung2_Mi> let x = Something           ...doesn't work
-}

--Something as type- and dataconstructor -> separate name spaces
-- data Something = Something 

exmplFunction :: Something -> Bool
exmplFunction This = True
exmplFunction That = False

pack::[Char] -> [[Char]]
pack [] =[]
pack (x:xs) = aux [x] xs
  where aux sorted [] = [sorted]
        aux (x:sorted) (y:unsorted) 
         | x==y = aux (x:y:sorted) unsorted
         | otherwise = (x:sorted): aux [y] unsorted

encode, encode' ::[Char] -> [(Int, Char)]
encode [] = []
encode (x:xs) = aux (1,x) xs
  where aux tuple [] = [tuple]
        aux (count, letter) (x:xs)
         | letter ==x = aux (count+1, letter) xs
         | otherwise = (count, letter): aux (1,x) xs

encode' xs = [(length ls, head ls) | ls <- pack xs]

decode::[(Int, Char)]->[Char]
decode [] =[]
decode ((count, letter): tuples) = times count letter ++ decode tuples
  where times count letter
          | count==0 = []
          | otherwise = letter : times (count-1) letter 

rotate:: [Int]-> Int-> [Int]
rotate [] _ = []
rotate xs 0 = xs
rotate list@(x:xs) count
   | count > 0 = rotate (xs ++[x]) (count-1)
   | otherwise = rotate (last list : init list) (count+1)

unword::[String] -> String
unword [] =""
unword (word:words) = word ++" "++ unword words

words::String->[String]
words [] =[]
words (x:xs) = aux [x] xs
  where aux word [] = [word]
        aux word (y:characters) 
         | y /= ' ' = aux (word++[y]) characters
         | otherwise = word : aux [y] characters

max_length, max_length', max_length'' ::[[Int]]-> Int
max_length [] = 0
max_length (xs:xss) = max (length xs) (max_length xss)

max_length' xss = maximum[length xs| xs<-xss]

max_length'' xss = (maximum.map length) xss 



