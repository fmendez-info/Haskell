
fact::(Integral a)=>a -> a
fact 0 = 1
fact n = n * fact (n-1)

cuenta::(Num t)=> [a] -> t 
cuenta []  = 0
cuenta (x:t) = 1 + cuenta t

suma::(Num t)=> [t] -> t
suma [] = 0
suma (x:t) = x + suma t

reversex::[a] -> [a]
reversex [] =  []
reversex (x:t) = reversex t ++ [x]

concatx::[a]->[a]->[a]
concatx [] l2 = l2
concatx l1 [] = l1
concatx (x:t) l = x: concatx t l 

minimo::(Ord a) => [a]-> a
minimo [] = error "No tiene minimo"
minimo [x]= x
minimo (x:y:t) = if x<=y then minimo(x:t) else minimo(y:t)

mtake :: Int -> [a] -> [a]
mtake _ [] = []
mtake 0 l  = []
mtake n (x:t) = x: take (n-1) t


mdrop :: Int -> [a] -> [a]
mdrop _ [] = []
mdrop 0 l  = l
mdrop n (x:t) = drop (n-1) t 

  