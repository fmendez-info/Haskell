data Arbol a = E | N (Arbol a) a (Arbol a)

completo::Arbol a -> Int -> Arbol a
completo e 0 = E
completo e n =   m  m m  m   m