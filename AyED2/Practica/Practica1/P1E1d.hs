--paresIguales :: Int → Int → Int → Int → Bool toma 4 n ́umeros enteros y retorna True si de
--dos en dos son iguales (en cualquier orden), en los dem ́as casos retorna False. Por ejemplo:
--paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
--paresIguales 3 1 1 3 = True

paresIguales :: Int -> Int -> Int -> Int -> Bool
--paresIguales a b c d = (a == b && c == d) || (a == c && b == d) || (a == d && b == c)
paresIguales a b c d | a == b && c == d = True
                     | a == c && b == d = True
                     | a == d && b == c = True
                     | otherwise = False
--OK