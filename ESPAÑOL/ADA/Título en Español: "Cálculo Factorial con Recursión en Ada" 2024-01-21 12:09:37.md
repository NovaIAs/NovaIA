```ada
with Ada.Text_IO;         use Ada.Text_IO;

procedure Factorial is

   function Factorial_Rec (N : Positive) return Positive is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial_Rec (N - 1);
      end if;
   end Factorial_Rec;

   X : Positive := 5;
begin
   Put (Factorial_Rec (X));
end Factorial;
```

Explicación:

* El código define una función `Factorial_Rec` que calcula el factorial de un número entero positivo.
* La función `Factorial_Rec` se llama recursivamente a sí misma hasta que llega a 0, en cuyo caso devuelve 1.
* El código también define una variable `X` de tipo `Positive` y le asigna el valor 5.
* La función `Factorial_Rec` se llama con el valor de `X` como argumento y el resultado se imprime en la consola.

El código es complejo porque utiliza la recursión, que es un concepto avanzado de programación. La recursión es la capacidad de una función de llamarse a sí misma. Esto puede ser útil para resolver problemas que se pueden dividir en subproblemas más pequeños del mismo tipo. En este caso, el problema de calcular el factorial de un número se puede dividir en el problema de calcular el factorial del número anterior.