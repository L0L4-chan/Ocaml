false && (2 / 0 > 0);;
(* -: bool = false*)
(* como la primera condicion es false, no sigue evaluando y devuelve false*)


(*true && (2 / 0 > 0);;*)
(*Error por división entre cero*)
(* Exception : Division_by_zero*)
(* como la primera condicion es verdad, pasa a evaluar la segundo, y se encuentra con una division entre cero, pense que saltaria un error, pero salta una excepcion*)


true || (2 / 0 > 0);;
(*- : bool = true*)


(*false || (2 / 0 > 0);;*)
(* Exception : Division_by_zero*)
(* como la primera condicion es falsa, pasa a evaluar la segundo, y se encuentra con una division entre cero, pero salta una excepcion*)


let con b1 b2 = b1 && b2;;
(*val con : bool -> bool  -> bool = >fun>*)


let dis b1 b2 = b1 || b2;;
(*val dis : bool -> bool  -> bool = >fun>*)

(*con (1 < 0) (2 / 0 > 0);;*)
(* Exception : Division_by_zero*)
(* como la primera condicion es verdad, pasa a evaluar la segundo, y se encuentra con una division entre cero, pense que saltaria un error, pero salta una excepcion*)

(1 < 0) && (2 / 0 > 0);;
(* - : bool -> false*)

(*dis (1 > 0) (2 / 0 > 0);;*)
(* - : bool -> true   pense que al ser la primera verdad no evaluaria la segunda, no me di cuenta de que asocia por la derecha, la funcion seria bool -> (bool -> bool) por lo que la division entre cero es el primer argumento a evaluar*) 
(*Exception : Division_by_zero*)


(1 > 0) || (2 / 0 > 0);;
(* - : bool -> false*)


