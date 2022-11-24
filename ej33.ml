let g n = (n >= 0 && n mod 2 = 0) || n mod 2 = -1;;

let g1 n = if n >= 0
		then if n mod 2 = 0
			then true
			else if n mod 2 = -1
				  then true
				  else false
		else  if n mod 2 = -1
				then true
				else false;;
let g2 n = match n >= 0 with
	| true -> (match n mod 2 = 0 with
		  | true -> true
		  | false -> (match n mod 2 = -1 with 
			| true -> true
			| false -> false))
	| false -> match n mod 2 = -1 with 
			| true -> true
			| false -> false;;


