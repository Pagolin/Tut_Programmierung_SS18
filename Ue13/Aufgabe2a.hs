import Prelude hiding (any)

isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf [] ys = True
isSubseqOf xs [] = False
isSubseqOf (x:xs) (y:ys)| x == y = isSubseqOf xs ys
                        | otherwise = isSubseqOf (x:xs) ys


data Tree a = Node a [Tree a] deriving (Show)

isPathOf:: Eq a => [a] -> Tree a -> Bool
isPathOf (x:xs) (Node y ts) 
           | x == y = empty xs || any (isPathOf xs) ts
           | otherwise = False
isPathOf [] _ = False

-- Type b can be unified with Tree a 
any:: (b->Bool)-> [b] -> Bool
any condition (b:bs) = condition b || any condition bs
any condition [] = False

empty:: [c] -> Bool
empty [] = True
empty _ = False

exampleTree = Node 1 [Node 2 [Node 4 [], 
                              Node 5 [Node 9 [],
                                      Node 10 []]], 
                      Node 3 [Node 6 [], 
                              Node 7 [Node 11 []], 
                              Node 8 []]] 
