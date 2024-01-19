```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Fibonacci es
   function Calcular_Fibonacci (N : in Integer) return Natural is
      -- Calcula el número de Fibonacci N-esimo
      F: array (0 .. N) of Natural := (others => 0);

   begin
      F(0) := 0;
      F(1) := 1;

      for I in 2 .. N loop
         F(I) := F(I-1) + F(I-2);
      end loop;

      return F(N);
   end Calcular_Fibonacci;

   N: Natural := 0;

begin
   Integer_Text_IO.Get (N);
   Put_Line (Natural'Image(Calcular_Fibonacci(N)));
end Fibonacci;

```
Esto es un programa que calcula el número de Fibonacci para un número introducido por teclado por ejemplo si introducimos el número 6 el programa mostrará por pantalla el número de Fibonacci correspondiente, en este caso 8.

Explicación del código:

* La declaración `with Ada.Text_IO; use Ada.Text_IO;` importa la biblioteca de E/S de texto de Ada, que incluye funciones para leer y escribir en la consola.
* La declaración `with Ada.Integer_Text_IO;` importa la biblioteca de E/S de texto de números enteros de Ada, que incluye funciones para convertir números enteros a texto y viceversa.
* El procedimiento `Fibonacci` es el punto de entrada del programa.
* La función `Calcular_Fibonacci` calcula el número de Fibonacci para un número entero dado `N`. Utiliza un algoritmo iterativo para calcular el número de Fibonacci.
* La declaración `N: Natural := 0;` declara una variable llamada `N` de tipo `Natural` e inicializa su valor a 0.
* La llamada `Integer_Text_IO.Get (N);` lee un número entero de la consola y lo almacena en la variable `N`.
* La llamada `Put_Line (Natural'Image(Calcular_Fibonacci(N)));` escribe el número de Fibonacci calculado en la consola.