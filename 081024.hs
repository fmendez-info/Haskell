minimo :: Ord p => [p] -> p
minimo [] = error "Conjunto Vacio"
minimo [x] = x
minimo (x:y:t) = if x < y then minimo (x:t) else minimo (y:t)

--tomar :: Int t => t -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x:t) = x:tomar (n-1) t

--sacar :: Int t => t -> [a] -> [a]
sacar _ [] = []
sacar 0 l = []
sacar n (x:t) = sacar (n-1) t

negar n = -n

signo n = signum n