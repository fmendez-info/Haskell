


pares_adyacentes ::  [a] -> [(a,a)]
pares_adyacentes xs = zip xs (tail xs)

esta_ordenada :: Ord a => [a] -> Bool
esta_ordenada xs = and [x <= y | (x,y) <- pares_adyacentes xs]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

largo :: [a] -> Int
largo [] = 0
largo (x:xs) = 1 + largo xs

zigzag :: [(a,a)] -> [a]
zigzag = zig
zig [] = []
zig ((x,_):xs) = x:zag xs
zag [] = []
zag ((_,y):xs) = y:zig xs