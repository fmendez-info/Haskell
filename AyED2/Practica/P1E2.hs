type Texto = [Char]
type Posicion = Int
type Linea = (Texto,Posicion)

vacia :: Linea
vacia = ("",0)

moverIzq :: Linea -> Linea
moverIzq (xs,0) = (xs,0)
moverIzq (xs,c) = (xs,c-1)

moverDer :: Linea -> Linea
moverDer (xs,c) | c < m = (xs,c+1)
                | otherwise = (xs,c)
                where m = length xs

moverIni :: Linea -> Linea
moverIni (xs,_) = (xs,0)

moverFin :: Linea -> Linea


insertar :: Char -> Linea -> Linea


borrar :: Linea -> Linea


