newtype Set a = Set { contains :: a -> Bool }

setSuchThat :: (a -> Bool) -> (Set a)
setSuchThat = Set

insert :: (Eq a) => a -> Set a -> Set a
insert x s = Set (\ x' -> x == x' || (contains s x'))

unionSet :: Set a -> Set a -> Set a
unionSet s1 s2 = Set (\z -> (contains s1 z) || (contains s2 z))

intersectSet :: Set a -> Set a -> Set a
intersectSet s1 s2 = Set (\z -> (contains s1 z) && (contains s2 z))

memberSet :: Set a -> a -> Bool
memberSet s e = (contains s e)

complementSet :: Set a -> Set a
complementSet s1 = Set(\z -> ((contains s1 z) == False))