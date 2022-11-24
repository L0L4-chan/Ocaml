

let  rec rev_append l1 l2 = match l1 with
[]-> l2
|h::t -> rev_append t (h::l2) 			

let rev l =  rev_append l []

let rec remove a = function 
[]->[]
|h::t-> match h=a with
	true -> t
	|false -> h::(remove a t)

let rec remove_all a = function 
[]->[]
|h::t-> match h=a with
	true -> remove_all a t
	|false -> h::(remove_all a t)


let rec ldif l1 = function
[] -> l1
|h::[] -> remove_all h l1
|h::t -> let m = remove_all h l1 in 
	 ldif m t 


let lprod l1 l2 = 
	 let rec aux_lprod la stg = 	
		match la with
		[]->  rev stg
		|h::t -> let rec aux2 la2 stg2 =	
			 match la2 with	
			 []-> stg2
			 |h1::t1-> aux2 t1 ((h,h1)::stg2)
			in 
			aux_lprod t (aux2 l2 stg) 	
		
	in aux_lprod l1 []
		

let divide l =  let rec aux_divide l a l1 l2  = 
			match l with
				[] -> ((rev l1), (rev l2))
				|h::t-> match a mod 2 with
					0 -> aux_divide t (a+1) l1 (h::l2)
					|_ -> aux_divide t (a+1) (h::l1) l2
		  in aux_divide l 1 [] []











