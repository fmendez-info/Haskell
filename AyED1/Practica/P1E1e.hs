derive :: Fractional a => (a -> a) -> a -> a -> a
derive f x dx = (f (x+dx) - f x) / dx

funcion:: Double -> Double
funcion x = 1/x