
(*curry : ((’a * ’b) -> ’c) -> (’a -> (’b -> ’c))*)
(*uncurry a, b : (’a -> (’b -> ’c)) -> ((’a * ’b) -> ’c)*)


let curry f x y = f (x, y)
(*val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun>*)
let uncurry f (x, y) = f x y ;;
(*val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun>*)



let ej =  uncurry (+);;
(*- : int * int -> int = <fun>*)

let sum = (uncurry (+));;
(*val sum : int * int -> int = <fun>*)

(*sum 1
Line 1, characters 4-5:
Error: This expression has type int but an expression was expected of type int * int *)

sum (2,1);;
(*- : int = 3*)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*val g : int -> int -> int = <fun>*)

(*g (2,5)
Line 1, characters 2-7:
Error: This expression has type 'a * 'b but an expression was expected of type int*)

let h = g 2;;
(*val h : int -> int = <fun>*)

h 1, h 2, h 3
(*- : int * int * int = (7, 10, 13)*)


let comp f g = function x -> f(g x);;

(*comp : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)*)

let f = let square x = x * x in comp square ((+) 1);;
(*val f : int -> int = <fun>*)

f 1, f 2, f 3
(* - : int * int * int = (4, 9, 16) *)

let i = function a-> a
let j = function (a,b) -> a 
let k = function (a,b) -> b
let l = function a-> [a]

