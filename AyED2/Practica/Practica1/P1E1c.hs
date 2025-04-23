--serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su tipo mas general.
no se

serie :: [a] -> [[a]]
serie [] = []
serie [x] = []:[[x]]
serie (x:xs) = [] : [x] :serie xs