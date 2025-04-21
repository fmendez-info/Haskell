inserta :: Ord a => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs)
                | a < x =  a:x:xs
                | otherwise = x:inserta a xs