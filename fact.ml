let rec fact = function
0 -> 1
| n -> n * fact (n - 1)  (*copiamos la funcion dada*)

let _ = if Array.length Sys.argv = 2  (*comprobamos que se pasan los parametros adecuados*)
	   then let a = int_of_string Sys.argv.(1) (* convertimos el string en un entero para usar la funcion*)
		in let c = string_of_int (fact a) (* Ahora convertimos la salida de la funcion en a (por eso en parentesis) en string *)
		in print_endline  c (*imprimimos el resultado*)
	else print_string "Entrada incorrecta\n" (*si el condicional no se cumple imprime mensaje de error*)
