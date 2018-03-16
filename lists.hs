import Data.List

seal :: (Ord a) => [a] -> [a] -> [a]
seal xs [] = xs
seal [] xs = xs
seal (h:first) (c:second) | h <= c = h:seal first (c:second) | h > c = c:seal (h:first) second



isSublist :: [a] -> [a] -> Bool
isSublist [] [] = True
isSublist [] right = True
isSublist left [] = False
isSubList (x:xs) (y:ys) | (x == y) = isSubList xs ys | otherwise = isSubList (x:xs) ys

combinator :: [a] -> [b] -> [(a,b)]
combinator xs [] = []
combinator [] ys = []
combinator xs ys = [ (x, y) | x <- xs, y <- ys]