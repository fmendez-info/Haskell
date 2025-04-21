
-- parte A -------------------------
inserta :: Ord a => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) = if a < x then
                        a:x:xs
                    else
                        x:inserta a xs

-- parte B -------------------------
particion :: Ord a => a -> [a] -> ([a],[a])
particion p [] = ([],[])
particion p (x:xs) = if x <= p then
                            (x:l1, l2)
                        else
                            (l1, x:l2)
                        where (l1,l2) = particion p xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores
                where (menores,mayores) = particion x xs