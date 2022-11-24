

let f1 n = match n mod 2 with
	  | 0 -> n/2
	  | _ -> n * 2;;

let f2 n =  match n mod 2 = 0 with
	  | true -> "es par" 
	  | false -> "es impar" ;;

let f3 n =  match n mod 2 = 0 with
	  | true -> "es multiplo de 2" 
	  | false -> match n mod 3 = 0 with
		| true -> "es multiplo de 3"
		| false -> " es impar" ;;
