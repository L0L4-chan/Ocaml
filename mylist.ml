
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


let rec nth  = function 
[] -> raise(Failure "nth")
|h::[] -> (function 
	0 -> h
	|a -> raise (Failure "nth"))
|h::t ->function 
	0 -> h
	|a ->  (nth t (a-1))

let rec append = function 
  [] -> (function
		[] -> []
		|l -> l )
| h :: t -> (function 
		[] -> h::t
		|l -> h:: append t l)



(*devuelve una lista con los resultados de la funcion f sobre los numeros correlativos hasta n-1*)
let rec init n f = match n with
0 -> [ ]
|1 -> [f 0]
|a -> append (init (a-1) f)  [f (a-1)]

(*Devuelve la lista invertida*)
let rec rev = function
[]->[]
|h::t -> append (rev t) [h]


(*Devuelve la primera lista invertida unida a la segunda lista*)
let rec rev_append l1 l2 = match l1 with
[]-> l2 
|h::t -> let m = append [h] l2 in
	rev_append t m

(*concatena una lista de listas *)
let rec concat l = match l with
[]->[]
|h::t -> match h with
	[] -> concat t
	|h1::t1-> append h (concat t)

(*concatena una lista de listas *)
let rec flatten l =  match l with
[]->[]
|h::t -> match h with
	[] -> flatten t
	|h1::t1-> append h (flatten t)

(*dada una funcion y una lista devuelve una lista con los resultados de f(a), f(b)....*)
let rec map f l = match l with
[] -> []
|h::t -> f h :: map f t


(*dada una funcion y una lista devuelve una lista con los resultados de f(n), f(n-1)....*)
let rev_map f l=  match l with
[] -> []
|_ -> rev (map f l)


(*dada una funcion y dos lista devuelve una lista con los resultados de f(a1 b1), f(a2 b2)....*)
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


(*devuelve el primer elemento que cumple la condicion f*)
let rec find f l = match l with
[] -> raise(Not_found)
|h::t -> match f h with 
	  true -> h
	 |false -> find f t

(*comprueba si todos los elementos cumplen la condicion f*)
let rec for_all f l = match l with
[] -> true
|h::t -> match f h with
	false -> false
	| true -> for_all f t


(*comprueba si algun elemento existente cumple f*)
let rec exists f l = match l with
[] -> false
|h::t -> match f h with
	true -> true
	| false -> exists f t


(*comprueba si un elemento existe*)
let rec mem a l = match l with
[] -> false
|h::t -> match h=a with
		 true -> true
		|false-> mem a t

(*devuelve una lista con todos los elemento que cumplen f*)
let rec filter f l =match l with
[] -> []
|h::t -> match f h with
	 true -> h :: filter f t
	|false -> filter f t

(*devuelve una lista con todos los elemento que cumplen f*)
let rec find_all f l =match l with
[] -> []
|h::t -> match f h with
	 true -> h :: find_all f t
	|false -> find_all f t


(*devuelve un par de listas una que cumple f y otra que no*)
let rec partition f l = match l with
[] -> [],[]
|h::t -> let aux = partition f t in 	
        match f h with 
	true -> h::fst aux,snd aux
 	|false -> fst aux, h::snd aux


(*de una lista de pares, devuelve un par de listas  a con el 1 miembro de cada par y b con el segundo miembro de cada par*)
let rec split = function
[] ->[],[]
|h::t-> let aux = split t in 
	let a, b = h in
		a:: fst aux, b:: snd aux


(*de dos listas a y b, devuelve una listas de pares a1,b1*)
let rec combine = function
[]-> raise(Not_found)
|h::t -> function
	[]-> raise(Not_found)
	|h1::t1 -> (h,h1):: combine t t1










