(*funcion recursiva en la que siendo y >= 0, power x y tenga el valor de x^y*)

let rec power x y = if y = 0 then 1
		else if y = 1 then x
		    else x * power x (y-1) 

(*x^y = (x * x)^(y/2); si y es par
  
   si x^y es x*x*x... y veces, al elevar x*x en lugar de una sola x estamos dividiendo a la mitad el numero de veces que debemos multiplicarlo
Ej: siendo  y = 4
  x^y seria x*x*x*x
  x*x)^(y/2) sería (x*x)*(x*x)
 x*x*x*x = (x*x)*(x*x)
siendo x=2 tendriamos
2*2*2*2 = 16
(2*2)*(2*2)= 4*4=16

x^y = x * (x * x)^(y/2); si y es impar

Dado que cualquier numero impar pasara a ser par restandole uno, y cualquier numero par , se convertirá en impar añadiendole uno, podemos concluir que, en base a la propiedad anterior
 x*(x*x)^y/2
sera valido para los casos impares.
Ej: siendo  y = 5
  x^y seria x*x*x*x*x
  (x*x)^(y/2) sería (x*x)*(x*x) por lo que requeririamos multiplicarlo una vez más por x para que fuera igual al anterior
  x*(x*x)^y/2
en este caso podemos ver que 
 x*x*x*x*x = x*(x*x)*(x*x)
siendo x=2 tendriamos
2*2*2*2*2 = 32 
2*(2*2)*(2*2)=2*4*4=32

*)



let rec power' x y = if y = 0 then 1
		else if y = 1 then x
		else if y mod 2 = 0 
		     then power'(x*x) (y/2)
		     else x * power'(x*x) (y/2)

(*power' reduce el numero de llamadas recursivas mediante division, mientras que power, las reduce mediante substracción por lo que el decrecimiento del valor y, que es quien marca el caso base o no recurrente en más lento en power.

Por lo que es menos eficiente dado que requiere más operaciones, más llamadas a la recursión y ocuparia más espacion en la pila recursiva, haciendo que el mayor valor de x que se puede utilizar en la llamada a esta funcion sea menor que en el caso de power'.
*)

let rec powerf x y = if y = 0 then 1.
		else if y = 1 then x
		else if y mod 2 = 0 
		     then powerf(x*.x) (y/2)
		     else x *. powerf(x*.x) (y/2)
