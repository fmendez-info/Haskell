--upto :: Int → Int → [Int] que dado dos numeros enteros n y m devuelve la lista [n, n + 1, n + 2, ..., m ]
--en caso que n <= m y la lista [] en otro caso. No usar listas por comprension.

upto :: Int -> Int -> [Int]
upto n m | n == m = [n]
         | n < m = n : upto (n+1) m
         | otherwise = []
--upto n m 
--    | n <= m = n : upto (n+1) m
--    | otherwise = []

--OK