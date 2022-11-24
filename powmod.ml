let rec power' x y = if y = 0 	
		     then 1
		     else   if y = 1 
			    then x
			    else   if y mod 2 = 0 
		     		   then power'(x*x) (y/2)
		                   else x * power'(x*x) (y/2)

let powmod m b e = if  b = 0 then 0
		else if e = 0 then 1 mod m
			else if  e = 1 then b mod m
				else if e mod 2 = 0 then let x = power' ((b * b) mod m) (e/2) in
							x mod m
					else let x = power' ((b * b) mod m) (e/2) in
						((b mod m) * x ) mod m  

