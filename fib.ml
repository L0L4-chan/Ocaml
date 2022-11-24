let rec fib n = if n <= 1
	 then n
	 else fib (n-1) + fib (n-2);;

let rec fib1 n = match 1 = n with (*Comprobamos si n es igual a uno*)
	| false -> fib1 (n-1); (* de no ser asi llamamos a esta funcion con n-1*)
		   print_endline( string_of_int (fib n))(*imprimimos el string de la salida de fib de  parametro*)
	| true -> print_endline( string_of_int (fib n));;(*si es uno, imprimimos la salida de fib del parametro*) 
	

let _ = if Array.length Sys.argv = 2  (*comprobamos que se pasan los parametros adecuados*)
	   then let x = Sys.argv.(1) in
		fib1(int_of_string x) (* llamamos a la funcion con el parametro dado convertido en entero*)
	   else print_string "Entrada incorrecta\n" (*si el condicional no se cumple imprime mensaje de error*)
