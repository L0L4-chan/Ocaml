
(* funcion que obtiene la suma de todas las cifras de la representacion decimal
del nuumero n. *)
let rec sum_cifras n = if n<10 then n 
		else n mod 10 + sum_cifras(n/10) 

(*funcion dada
let num_cifras n = String.length (string_of_int (abs n))*)
(*funcion recursiva*)
let rec num_cifras n = if  n< 10 then 1
		else 1 + num_cifras(n/10) 

(*Funcion exp10 dado un n devolvera 10^n de forma recursiva*)

(*let rec exp10  = function 0 -> 1 | 1-> 10 | n -> 10 * exp10(n-1) *)

let rec exp10 n = if n = 0 then 1
		else if n = 1 then 10
			else 10 * exp10(n-1)

(*funcion que dado un numero entero devuelve el entero que se obtiene al invertir el orden de las cifras de la representacion decimal*)

let rec reverse n = if n < 10  then n
		else 
			let a = string_of_int(n mod 10) ^ string_of_int (reverse (n/10)) in
				int_of_string a
			
			
(*indica si el string sobre el que se aplica se lee igual de izquierda a derecha que de derecha a
izquierda *)

let rec palindromo s = if String.length s -1 <= 0 then true
		else   if s.[0] = s.[String.length s -1] 
			then  palindromo ( String.sub s 1 (String.length s -2))
	               	else false
