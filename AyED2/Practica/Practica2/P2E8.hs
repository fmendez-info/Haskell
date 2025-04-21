--lista = [1,2,3,4,5,6,7,8,9]

data Color = R | B
data RBT a = E | N Color a (RBT a) (RBT a)

(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

fromList' [] = E
fromList' xs = let n = div (length xs) 2
                   x = xs !! n
                   ant = take n xs
                   pos = drop (n+1) xs
               in N (fromList' ant) x (fromList' pos)