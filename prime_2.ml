let is_prime n =
	let rec check_from i =
	i >= n ||
	(n mod i <> 0 && check_from (i+1))
	in check_from 2;;


let rec next_prime n = if is_prime n
	then n
	else next_prime (n+1);;

let rec last_prime_to n = if is_prime n
	then n
	else last_prime_to (n-1);;


let is_prime2 n =( match n>=2 with
			| true -> false (*si es 2 1 0 no es primo*)
			| false -> (match n mod 2 = 0 with (*comprobamos que no sea divisible por 2*)
				    | true -> false
				    | false -> let x = 3 in
						for x to x*x<=n do
							 (match n mod x == 0 with
								|true -> false
								|false -> x = x + 2 )
	    					done;
						true))	
