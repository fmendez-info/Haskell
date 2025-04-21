-------------------
--- Selection Sort

delete y [] = []
delete y l = [x|x<-l,x /= y]

minimo [x] = x
minimo (x:y:t) = if x < y then minimo (x:t)
                else minimo (y:t)

ssort [] = []
ssort [x] = [x]
ssort l = m:ssort l'
  where
    m = minimo l
    l' = delete m l
----------------------------------------
--- Insertion Sort

insert x [] = [x]
insert x (y:t) = if x < y then (x:y:t)
                 else y:insert x t

isort [] = []
isort [x] = [x]
isort (x:t) = insert x (isort t)

----------------------------------------
--- Quick Sort

qsort [] = []
qsort [x] = [x]
qsort (x:t) = qsort menores ++ [x] ++ qsort mayores
  where
    menores = [j | j<-t, j < x]
    mayores = [j | j<-t, j >= x]
--- Version 2
qsort2 [] = []
qsort2 [x] = [x]
qsort2 (x:t) = let
  menores = [j | j<-t, j < x]
  mayores = [j | j<-t, j >= x]
  in
    qsort2 menores ++ [x] ++ qsort2 mayores

----------------------------------------
--- Merge Sort

split [] = ([],[])
split [a] = ([a], [])
split (a:b:t) = let
  (m,n) = split t
  in
    (a:m, b:n)
---------
split2 [] = ([],[])
split2 [a] = ([a], [])
split2 (a:b:t) = (a:m, b:n)
  where
    (m,n) = split2 t
---------
merge [] [] = []
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys) =
  if x < y then x:merge xs (y:ys)
  else y:merge (x:xs) ys
---------
msort [] = []
msort [x] = [x]
msort lista =
  let
    (i,j) = split lista;
    i' = msort i;
    j' = msort j
  in
    merge i' j'
--------------------------------
--- Previamente ver el funcionamiento de
--- La funcion de orden superior "foldr"

--- Observar que fold necesita una funcion de dos variables
--- ej.: suma x y = x+y
----foldr suma 0 [2,3,6,8,67] ..2+(3+(6+(8+(67+0))))

isort2 [] = []
isort2 [x] = [x]
isort2 (l) = foldr insert [] l

---------
msort2 [] = []
msort2 [x] = [x]
msort2 l = foldr merge [] [[x]|x<-l]
