
let  cal_e = (function x ->(1.+. (1./. x))**x) (*definicion de la funcion*)
let x = 99999999.99 (*asignamos un valor a x*)
let e = cal_e x  (*Calculamos y guardamos el resultado en e*)
let () = print_float e (*imprimimos*)
let () = print_endline "" (*imprimimos espacio*)

