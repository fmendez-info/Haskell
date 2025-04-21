--quick sort

qsort [] = []
qsort []



--merge sort

split [] = ([],[])
split [a] = ([a],[])
split (a:b:t) = let
    (m,n) = split t
    in
        (a:m, b:n)
------------
merge [] [] = []
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys) = 
    if x < y then x:merge xs (y:ys)
    else y:merge (x:xs) ys
------------
msort [] = []
msort [x] = [x]
msort lista =
    let
        (i,j) = split lista