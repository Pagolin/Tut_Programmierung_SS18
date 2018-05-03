module Uebung3_Mi where

data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving Show

exampleTree = Branch 1 (Branch 2 (Branch 4 (Leaf 5) (Leaf 6)) 
                                 (Leaf 7))
                       (Branch 3 (Leaf 8) (Leaf 9))

depth::Tree a -> Int
depth (Leaf _) = 1
depth (Branch a left right) = 1 + (min (depth left) (depth right))

paths:: Tree a -> Tree [a]
paths tree = accu [] tree
       where accu path (Leaf x) = Leaf (path ++[x])
             accu path (Branch x left right) = Branch (path++[x]) (accu (path++[x]) left) (accu (path++[x]) right)

tmap::(a->b) -> Tree a -> Tree b
tmap f (Leaf x) = Leaf (f x)
tmap f (Branch x left right) = Branch (f x) (tmap f left) (tmap f right)

--Produkt(Quadrate(geradeZahlen(Liste)))
f::[Int]-> Int
f xs =   foldr (*) 1 (map (\x-> x*x) (filter even xs))

g::[Int]-> Int
g xs =   foldr (*) 1 (map (^2) (filter even xs))

f_composed::[Int] -> Int
f_composed = foldr (*) 1 . map square . filter even
    where square x = x*x


