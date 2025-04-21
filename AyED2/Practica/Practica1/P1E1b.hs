--collect [('a',1),('c',5),('a',4),('b',2)]
--devuelve [('a',[1,4]),('c',[5]),('b',[2])]

elementos :: k -> [(k,v)] -> [v]
elementos e [] = []
elementos e ((x,y):xs)  | e == x = y : elementos e xs
                        | otherwise = elementos e xs

borrar :: k -> [(k,v)] -> [(k,v)]
borrar e [] = []
borrar e ((x,y):xs) | e == x = borrar e xs
                    | otherwise = (x,y) : borrar e xs

collect [] = []
collect ((x,y):xs) = (x, y : elementos x xs) : collect (borrar x xs)