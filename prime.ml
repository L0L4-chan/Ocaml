(*funcion para calcular el numero primo dada*)
let is_prime n =
	let rec check_from i =
	i >= n ||
	(n mod i <> 0 && check_from (i+1))
	in check_from 2;;

(*funcion para devolver el numero primo mas cercano por la cota superior*)
let rec next_prime n = if is_prime n
	then n
	else next_prime (n+1);;
(*funcion para localizar el numero primo mas cercano en la cota inferior*)

let rec last_prime_to n = if is_prime n
	then n
	else last_prime_to (n-1);;

(*funcion recursiva para la funcion prime2*)
let rec check i x = (if i*i >= x (*Criba de erastostenes para escoger el divisor*)
			then 
			   (if x mod i = 0 (*si es posible divisor comprobamos*)
				then false
				else check (i+2) x) (*si no da cero aumentamos el divisr en la siguiente llamada*)
			else true)

(*nueva funcion para  calcular si es numero primo*)
let is_prime2 n = match n>=2 with
			| true -> false (*si es 2 1 0 no es primo*)
			| false -> match n mod 2 = 0 with (*comprobamos que no sea divisible por 2*)
				    | true -> false (*no es primo*)
				    | false -> let i=3 in check i n(*probamos otros posibles divisores*)	
