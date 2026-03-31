--eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
--veces como indica su posicion. No usar listas por comprension.
--Por ejemplo: eco "hola" = "hoolllaaaa"

repetir :: Int -> Char -> [Char]
repetir 0 _ = ""
repetir n c = c : repetir (n-1) c

indices' :: [Char] -> [(Char,Int)]
indices' xs = zip xs [1..]

--eco :: [Char] -> [Char]
--eco [] = []
--eco (x:xs) = repetir i a ++ eco xs
--    where  = indices' (x:xs)

--eco xs = concat (map (\(i, x) -> repetir i x) (zip [1..] xs))
--eco xs = concat (map (         f              ) (    lista   ))
--eco xs = concat (zipWith replicate [1..lenght xs] xs)

eco :: String -> String
eco xs = eco' 1 xs
    where
        eco' _ [] = []
        eco' n (x:xs) = rep x n ++ eco' (n+1) xs
            where
                rep c 0 = []
                rep c m = c : rep c (m - 1)