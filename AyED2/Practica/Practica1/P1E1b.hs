--collect :: [(k, v)] → toma un lista de pares (clave, valor) y asocia cada clave unica con todos los valores con los que estaba apareada originalmente
--collect [('a',1),('c',5),('a',4),('b',2)]
--devuelve [('a',[1,4]),('c',[5]),('b',[2])]

--hago una lista de todos los valores de una clave
valores :: Eq a => a -> [(a,b)] -> [b]
valores _ [] = []
valores e ((k,v):xs) | e == k = v : valores e xs --agrego el elemento y evaluo el resto
                     | otherwise = valores e xs  --no lo agrego y evaluo el resto

--borro todos los pares donde esta una clave (porque ya los use)
borrarParesClave :: Eq a => a -> [(a,b)] -> [(a,b)]
borrarParesClave _ [] = []
borrarParesClave e ((k,v):xs) | e == k = borrarParesClave e xs
                              | otherwise = (k,v) : borrarParesClave e xs

--agrego la tupla (k,[v]) y voy al resto (con los (k,x) ya borrados)
collect :: Eq a => [(a,b)] -> [(a,[b])]
collect [] = []
collect ((k,v):xs) = (k, v : valores k xs) : collect (borrarParesClave k xs)

--OK