--eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
--veces como indica su posicion. No usar listas por comprension.
--Por ejemplo: eco "hola" = "hoolllaaaa"

repetir :: Int -> Char -> [Char]
repetir 0 _ = ""
repetir n c = c : repetir (n-1) c

indices' :: [Char] -> [(Char,Int)]
indices' [] = []
indices' [x] = [(x,1)]
indices' (x:xs) = 

eco :: [Char] -> [Char]
eco [] = []
eco (x:xs) = repetir i x ++ eco xs
    where i = i+1