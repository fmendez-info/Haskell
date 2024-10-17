divisores n = [x | x <- [1..n], n `mod` x == 0]
primo x = divisores x == [1,x]

r = ["Hola", "es", "temprano"]

concat xxs = [x | xs<-xxs, x<-xs]
