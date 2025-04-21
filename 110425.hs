--foldr ::   f        e    xs    resultado
foldr' :: (b->a->a) -> a -> [b] -> a

foldr' f e [] = e
foldr' f e (x:xs) = f x (foldr f e xs)
