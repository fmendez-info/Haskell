divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

sumaDivisores :: Int -> Int
sumaDivisores n = sum [x | x <- divisores n, x /= n]

perfecto :: Int -> Bool
perfecto n = sumaDivisores n == n

perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], perfecto x]