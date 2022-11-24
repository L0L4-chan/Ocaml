
(*Algoritmo de euclides para el calculo del MCD*)
let rec mcd (x,y) = if   x = 0 then y
			else 	if  y = 0 then x
				 else  mcd(y,(x mod y))
