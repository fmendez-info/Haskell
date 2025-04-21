fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)


contar :: (Num p) => [a] -> p
contar [] = 0   
contar (x:xs) = 1 + contar xs

sumar :: (Num p) => [a] -> p
sumar [] = 0
sumar (x:xs) = 1 + sumar xs

revertir [] = []
revertir (x:xs) = revertir xs ++ [x]

--concatenar :: [a][a] -> [a]
concatenar [] l2 = l2
concatenar l1 [] = l1
concatenar (x:xs) l2 = x : concatenar xs l2
