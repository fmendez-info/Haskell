
{-
Un numero perfecto es aquel que es igual a la suma de sus divisores. 6 = 3+2+1
Usando listas por comprensión, escribir la función perfectos para obtener los números perfectos en el intervalo [1, n].
-}

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

sumaDivisores :: Int -> Int
sumaDivisores n = sum [x | x <- divisores n, x /= n]

perfecto :: Int -> Bool
perfecto n = sumaDivisores n == n

perfectos n = [x | x <- [1..n], perfecto x]

{-
Escribir una funcion que recibe como argumento dos listas orenadas y devuelve una lista ordenada fusion de las dos listas
juntar :: Ord a => [a] -> [a] -> [a]
-}

juntar :: Ord a => [a] -> [a] -> [a]
juntar a [] = a
juntar [] b = b
juntar (x:xs) (y:ys) = if x<y then
                            x : juntar xs (y:ys)
                        else
                            y : juntar ys (x:xs)


{-
Escriba una funcion Qsort :: Ord a => [a] -> [a]
Obs: Escriba una funcion particion que reciba como argumentos
un pivot y una lista de valores
Esta funcion da como resultado una tupla con 2 listas (l1,l2)
con l1 lista de los valores menores o iguales que el pivot
y l2 los mayores que el pivot
particion :: Ord a => a -> [a] -> ([a],[a])
-}

particion :: Ord a => a -> [a] -> ([a],[a])         -- con un elemento a y una lista de a me entrega una tupla de 2 listas de a
particion p [] = ([],[])                            -- particion sobre una lista vacia da 2 listas vacias (caso final)
particion p (x:xs) = if x <= p then                 -- tomo el primer elemento de la lista, si es menor o igual a p
                            (x:l1,l2)               -- lo agrego a l1
                        else                        -- sino
                            (l1,x:l2)               -- lo agrego a l2
                     where (l1,l2) = particion p xs -- defino que l1 y l2 son las listas resultantes de hacer particion recursiva
                                                    --  sobre el resto de la lista

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores
                where (menores,mayores) = particion x xs

{-
Escribir la funcion miZip
Recibe como argumentos 2 listas y produce una lista de tuplas
agarrando un elemento de una lista y uno de la otra
termina cuando se acaban los elementos de la lista mas corta
miZip :: [a] -> [b] -> [(a,b)]
-}

miZip :: [a] -> [b] -> [(a,b)]
miZip _ [] = []
miZip [] _ = []
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys

{-
Usando miZip y listas por comprension
Escribir una funcion que realice el producto escalar de 2 listas
Es la suma de los productos uno a uno
-}

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xs ys = sum [x*y | (x,y) <- miZip xs ys]

{-
Utilizando la funcion miZip
Escribir la funcion indexado
Dada una lista produce una lista de tuplas donde cada elemento de la lista
tiene su posicion dentro de la misma. La indexacion empieza en 1.
Ej: indexado ["Juan","Pedro","Luis"] = [(Juan,1),(Pedro,2),(Luis,3)]
-}

indexado :: [a] -> [(a,Int)]
indexado lista = miZip lista [1..]

{-
Escribir una funcion qeu inserta elementos en una lista manteniendola ordenada
inserta :: Ord a => a -> [a] -> [a]
-}

inserta :: Ord a => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) = if a < x then
                        a:x:xs
                    else
                        x:inserta a xs

{-
Considerando la siguiente funcion
split::Ord a => a->[a]->([a],[a])
split x l = ( [y | y <- l, y <= x] , [y | y <- l, y > x] )
Defina una version de esta funcion que trabaje en exactamente
una sola pasada a la lista l
-}

split :: Ord a => a -> [a] -> ([a],[a])
split _ [] = ([],[])
split x (h:t) = if h <= x then
                    (h:menores,mayores)
                else
                    (menores,h:mayores)
                where (menores,mayores) = split x t

-------------------------------TDA-------------------------------------------------

{-
Defina un tipo de dato arbol binario de busqueda ArbolBin
Escriba el metodo addTree e inOrderTree
addTree :: Ord a => a -> ArbolBin a -> ArbolBin a
Inserta un elemento de tipo a en un arbol binario
inOrderTree :: Ord a => ArbolBin a -> [a]
Produce un listado en orden del arbol binario
-}

data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a) deriving Show

nuevoArbol  :: Ord a => ArbolBin a
addTree     :: Ord a => a -> ArbolBin a -> ArbolBin a
inOrderTree :: Ord a => ArbolBin a -> [a]

nuevoArbol = Vacio

addTree x Vacio = Nodo x Vacio Vacio
addTree x (Nodo n izq der)
            | x==n = Nodo n izq der
            | x<n  = Nodo n (addTree x izq) der
            | x>n  = Nodo n izq (addTree x der)

inOrderTree Vacio = []
inOrderTree (Nodo n izq der) = inOrderTree izq ++ [n] ++ inOrderTree der

{-
Una cola de prioridad es una estructura que almacena elementos clasificables
Con la particularidad que cuando se saca un elemento siempre se extrae el menor
La prioridad mas baja primero
mkqpr: instanciar una nueva cola vacia
addqpr: agregar un nuevo elemento a la cola
nextqpr: devuelve el elemento mas bajo de la cola
popqpr: devuelve una cola donde se quito el menor
Definir el tipo ColaPrioridad e implementarlo usando    una lista
                                                        un arbol binario de busqueda como estructura
Escribir todas las funciones para manipular el arbol
-}

newtype ColPri c = CP [c] deriving Show

mkqpr   :: Ord a => ColPri a
addqpr  :: Ord a => a -> ColPri a -> ColPri a
nextqpr :: Ord a => ColPri a -> a
popqpr  :: Ord a => ColPri a -> ColPri a

mkqpr = CP[]

addqpr a (CP xs) = CP (insertar a xs)
            where
                insertar a [] = [a]
                insertar a (x:xs)
                        | a<=x = a:x:xs
                        | a>x  = x:insertar a xs

nextqpr (CP []) = error "cola vacia"
nextqpr (CP (x:_)) = x

popqpr (CP []) = error "cola vacia"
popqpr (CP (_:xs)) = CP xs


data Arbol a = Vacio2 | Nodo2 a (Arbol a) (Arbol a) deriving Show
newtype ColaP a = CP2 (Arbol a) deriving Show

-- funciones Arbol
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Vacio2 = Nodo2 x Vacio2 Vacio2
insertar x (Nodo2 n izq der)
            | x==n = Nodo2 n izq der
            | x<n  = Nodo2 n (insertar x izq) der
            | x>n  = Nodo2 n izq (insertar x der)

minimo :: Ord a => Arbol a -> a
minimo (Nodo2 n Vacio2 _) = n
minimo (Nodo2 _ izq _) = minimo izq

eliminarMin :: Ord a => Arbol a -> Arbol a
eliminarMin (Nodo2 _ Vacio2 der) = der
eliminarMin (Nodo2 n izq der) = Nodo2 n (eliminarMin izq) der 

-- funciones ColaP

mkqpr2   :: Ord a => ColaP a
addqpr2  :: Ord a => a -> ColaP a -> ColaP a
nextqpr2 :: Ord a => ColaP a -> a
popqpr2  :: Ord a => ColaP a -> ColaP a

mkqpr2 = CP2 Vacio2

addqpr2 x (CP2 arbol) = CP2 (insertar x arbol)

nextqpr2 (CP2 Vacio2) = error "cola vacia"
nextqpr2 (CP2 arbol) = minimo arbol

popqpr2 (CP2 Vacio2) = error "cola vacia"
popqpr2 (CP2 arbol) = CP2 (eliminarMin arbol)

-- prueba:
--      let cola = foldr addqpr mkqpr [5, 3, 8, 1, 4]
--      print cola
--      print (nextqpr2 cola)