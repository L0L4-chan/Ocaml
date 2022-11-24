let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1


let rec orbit n = if n = 1 	
		  then string_of_int n 
		  else 
			if n mod 2 = 0 
			then  string_of_int n ^ ", " ^  orbit(n/2)   
			else  string_of_int n ^ ", " ^  orbit(3 * n +1)

let rec length n = if n = 1 	
		  then 0 
		  else 
			if n mod 2 = 0 
			then  1 +  length(n/2)   
			else  1 +  length(3 * n +1)

let rec top = function 
		1 -> 1
		|n -> (if n mod 2 = 0
	 	      then  	
			   let a = top(n/2) in 
				(if n > a 
				then n
				else a)
		      else 	
			   let a = top (3 * n + 1) in
	 			(if n > a 
				then n
				else a))
 					 
let rec length'n'top = function 
			1 -> 0,1
			|n -> (if n mod 2 = 0
	 	      		then  let a,b = length'n'top(n/2) in 
				 	(if n > b 
					then (1+a,n)
					else (1+a,b))
		      		else let a,b = length'n'top(3 * n + 1) in
	 				(if n > b 
					then (1+a,n)
					else (1+a,b)))


(*Suponemos que los parametros aparecen como m el menor miembro del intervalo y n la cota superior del mismo*)
let rec longest_in  m n = (if n = 1 
			  then 1,0 
			  else 
				(if m < n 
				then 
					let a, b = longest_in (m+1) n in
					let aux_length = length m in
						(if  aux_length > b
						then 	(m, aux_length)
						else 
							(if aux_length = b
							then 	
								(if a < m
								then a,b
								else m,aux_length)
							else a,b))
							 
				else  n,length n))





let rec highest_in  m n = (if n = 1 
			  then 1,0 
			  else  (if m < n 
				then let a, b = highest_in (m+1) n in
					let aux_high = top m in
						(if  aux_high > b
						then 	(m, aux_high)
						else (if aux_high = b
							then (if a < m
							      then a,b
							      else m,aux_high)
							else a,b))
							 
				else  n,top n))









