--borrarUltimo que dada una lista borra el iltimo elemento de la lista. No utilizar reverse ni tail
borrarUltimo :: [a] -> [a]
borrarUltimo [] = error "Lista vacia"
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

--OK