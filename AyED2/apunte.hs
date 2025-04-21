{-
Mat       | Haskell
f(x)      | f x
f(x,y)    | f x y
f(g(x))   | f (g x)
f(x,g(y)) | f x (g y)
f(x)g(y)  | f x * g y
-}

--Asocia a la izquierda
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use guards" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
f a b = (f a) b

--Funciones con camelCase
miFuncion = 1

a = b + c
    where
        b = 1
        c = 2
d = a * 2

--True :: Bool
--False :: Bool
negar :: Bool -> Bool
negar True = False
negar False = True

--Tipos basicos
--Int = 1 (precision fija)
--Integer = 1 (sin limite)
--Float = 1.0 (precision fija)
--Double = 1.0 (sin limite)
--Char = 'a' (un solo caracter)
--Bool = True | False
--String = "Hola" = ['H','o','l','a']
--Lista = [1,2,3]
--Lista de listas = [[1,2,3],[4,5,6]]
--Lista de tuplas = [(1,'a'),(2,'b'),(3,'c')]
--Tupla = (1,'a',True)
--Tupla con tupla = (1,('a',True))
--Tupla de listas = ([1,2,3],['a','b','c'])
--Tupla de tuplas = ((1,'a'),(1,'b',False))

--Una funcion mapea valores de un tipo en otro
-- not :: Bool -> Bool
-- even :: Int -> Bool --Par
-- odd :: Int -> Bool  --Impar
-- succ :: Int -> Int  --Sucesor
-- pred :: Int -> Int  --Predecesor
-- abs :: Int -> Int   --Valor absoluto

add :: (Int,Int) -> Int
add (x,y) = x + y
zeroTo :: Int -> [Int]
zeroTo n = [0..n]

---------------------------------------------
--Currificacion: tomar los argumentos de a uno
--En Haskell todas las funciones son de un solo argumento
--La funcion toma el primer argumento
--Devuelve una funcion que toma el segundo
--Y esta devuelve el resultado final
--Permite aplicar parcialmente
suma :: (Int,Int) -> Int
--toma una tupla y devuelve un el resultado
suma (x,y) = x + y
sumar :: Int -> (Int -> Int)
--toma un entero y devuelve una funcion
sumar x y = x + y
sumar3 :: Int -> Int
sumar3 x = sumar 3 x

tomarPrimeros5 :: [a] -> [a]
tomarPrimeros5 xs = take 5 xs

multiplicar :: Int -> (Int -> (Int -> Int))
multiplicar x y z = x * y * z
--Como -> se asocia a la derecha:
--multiplicar :: Int -> Int -> Int -> Int
--((multiplicar x) y) z

-------------------------------------
--Funciones Polimorficas
--Funciones que pueden tomar diferentes tipos de argumentos
--Ejemplo: la funcion length
--largo :: [a] -> Int
--largo [False,True] --a = Bool
--largo ['a','b']    --a = Char

-------------------------------------
--Condicionales
--if <condicion> then <expresion1> else <expresion2>
--Ambas expresiones deben ser del mismo tipo
absoluto :: Int -> Int
absoluto n = if n > 0 then n else -n
--Se puede anidar if
signo :: Int -> Int
signo n = if n < 0 then -1 else
            if n == 0 then 0 else 1
signo2 :: Int -> Int
signo2 n | n < 0 = -1
         | n == 0 = 0
         | otherwise = 1




--Funciones definidas:
head' :: [a] -> a
head' [] = error "Lista Vacia"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

--map toma una lista de elementos a
--le aplica una funcion f (a->b)
--y devuelve una lista de elementos b
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

--concat concatena listas en una sola
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

--zip devuelve lista de tuplas (a,b)
--donde a viene de la 1er lista y b de la 2da
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

--pairs devuelve lista de pares de adyacentes
pairs :: [a] -> [(a,a)]
pairs xs = zip' xs (tail' xs)

indexes :: [a] -> [(a,Int)]
indexes xs = zip' xs [0..]

positions :: Eq a => a -> [a] -> [Int]
positions n xs = [ i | (x,i) <- indexes xs, x == n ]

factorial :: Int -> Int
factorial 0 = 1  --caso base
factorial n = n * factorial (n - 1)

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

reverse' ::[a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
--solo se puede usar :
--en el caso de elemento:lista
--no se puede hacer lista:elemento
--en ese caso se hace lista ++ [elemento]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores
    where menores = [a | a <- xs, a <= x]
          mayores = [b | b <- xs, b > x]


--safediv :: Int 

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x