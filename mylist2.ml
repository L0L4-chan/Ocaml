
let hd = function 
[] -> raise (Failure "hd") 
|a::_ -> a

let rec tl = function 
[] -> raise (Failure "tl")
|_::t -> t

let rec length  = 
	let rec aux i = function 
	[]-> i
	|_::t-> aux (i+1) t
	in 
     function l -> aux 0 l

let rec compare_lengths = function
[] -> (function []  -> 0
	|_-> -1)
|_:: t1 ->(function [] -> 1
	| _::t2 -> compare_lengths t1 t2)

let rec nth l a = match l with 
[] -> raise(Failure "nth")
|h::[] -> (match a <0 with
	 true ->raise(Invalid_argument "nth")
	|false-> (match a with  	
			0 -> h
			|a -> raise (Failure "nth")))
|h::t -> match a<0 with
	true ->raise (Invalid_argument "init")
	|false->match a with  
			0 -> h
			|a ->  (nth t (a-1))

let rec append = function 
  [] -> (function
		[] -> []
		|l -> l )
| h :: t -> (function 
		[] -> h::t
		|l -> h:: append t l)

let init n f = match n<0 with
true -> raise (Invalid_argument "init")
|false-> let rec aux_init a l =
		match a<n  with
		 true -> aux_init (a+1) ((f (n-(a+1)))::l)
		 |false ->  l
		 
	in aux_init 0 []
			

let  rec rev_append l1 l2 = match l1 with
[]-> l2
|h::t -> rev_append t (h::l2) 			

let rev l =  rev_append l []

(* otra version vista en clase
let rev l = function
[]->[]
|h::t - >fold_left(fun a x -> x::a) [] l
*)

(*let  rec rev_append l1 l2 = match l1 with
[]-> l2
|h::t -> rev_append t (h::l2) *)

let rec concat l = match l with
[]->[]
|h::t -> match h with
	[] -> concat t
	|h1::t1-> append h (concat t)

let rec flatten l =  match l with
[]->[]
|h::t -> match h with
	[] -> flatten t
	|h1::t1-> append h (flatten t)

let rec map f l = match l with
[] -> []
|h::t -> f h :: map f t


(* recursiva terminal*)
(*dada una funcion y una lista devuelve una lista con los resultados de f(n), f(n-1)....*)
let rev_map f l=  
let rec aux_rev_map  l l1=
		match l with
		[]-> l1
		| h:: t -> aux_rev_map t (rev_append [f h] l1)
	in aux_rev_map  l []
		 

let rec map2 f l1 l2 = match l1,l2 with
[], [] -> []
|[], _ -> raise (Invalid_argument "map2")
|_,[] -> raise (Invalid_argument "map2")
|h::t,h1::t1-> f h h1 :: map2 f t t1


let rec fold_left f ac l = match l with
[]-> ac
|h::t ->  fold_left f (f ac h) t 

let rec fold_right f l ac= match l with
[]-> ac
|h::t -> f h (fold_right f t ac)

let rec find f l = match l with
[] -> raise(Not_found)
|h::t -> match f h with 
	  true -> h
	 |false -> find f t

let rec for_all f l = match l with
[] -> true
|h::t -> match f h with
	false -> false
	| true -> for_all f t

let rec exists f l = match l with
[] -> false
|h::t -> match f h with
	true -> true
	| false -> exists f t

let rec mem a l = match l with
[] -> false
|h::t -> match h=a with
		 true -> true
		|false-> mem a t



let find_all f l =
	let rec aux_find_all  l l1 = 
        	match l with
       		 [] -> rev l1
        	|h::t -> match f h with
	 		true -> aux_find_all  t (h::l1)
			|false -> aux_find_all  t l1
	in aux_find_all l []

let rec filter = find_all

let partition f l = 
 let rec partition_aux  l l1 l2  =	
          match l with 
	[] -> (rev l1),(rev l2)
 	|h::t -> match f h with
		true -> partition_aux t (h::l1) l2
		|false ->partition_aux t  l1 (h::l2)
 in partition_aux l [] []

let rec split = function
[] ->[],[]
|h::t-> let aux = split t in 
	let a, b = h in
		a:: fst aux, b:: snd aux

let rec combine = function
[]-> raise(Invalid_argument "combine")
|h::t -> function
	[]-> raise(Invalid_argument "combine")
	|h1::t1 -> (h,h1):: combine t t1

