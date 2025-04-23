--ror que dada una lista xs y un entero n, tal que n <= lenght xs, rota los primeros n elementos
--de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. Definir una version recursiva de ror
--sin usar drop, take ni tail.

ror :: Int -> [a] -> [a]
ror _ [] = []
ror _ [x] = [x]
ror 0 (x:xs) = x:xs
--ror n (x:xs) = ror (n-1) (xs ++ [x])
ror n (x:xs) | n <= length (x:xs) = ror (n-1) (xs ++ [x])
             | otherwise = error "n es mayor a la cantidad de elementos"

--OK