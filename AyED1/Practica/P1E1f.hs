signo :: Real a => a -> String
signo x = if x > 0 then "positivo" else if x < 0 then "negativo" else "cero"