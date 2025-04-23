--isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un triangulo
--dice si es un triangulo isosceles.

isoceles :: Int -> Int -> Int -> Bool
--isoceles a b c = (a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a)
--isoceles a b c | a == b && a /= c = True
--               | a == c && a /= b = True
--               | b == c && b /= a = True
--               | otherwise = False

isoceles a b c | a == b && a == c = False
               | a /= b && a /= c && b /= c = False
               | otherwise = True

--OK