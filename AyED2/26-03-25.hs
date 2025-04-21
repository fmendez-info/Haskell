type Par a = (a,a)

mult :: Par Int -> Int
mult (x,y) = x * y

copy :: a -> Par a
copy x = (x,x)

