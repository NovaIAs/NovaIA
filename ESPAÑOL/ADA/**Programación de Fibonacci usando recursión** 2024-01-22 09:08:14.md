```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Fibonacci is

   function Fibonacci_Rec (N : in Positive) return Natural is
   begin
      if N <= 2 then
         return 1;
      else
         return Fibonacci_Rec (N - 1) + Fibonacci_Rec (N - 2);
      end if;
   end Fibonacci_Rec;

begin
   Ada.Text_IO.Put_Line ("Ingrese un número positivo:");
   Ada.Integer_Text_IO.Get (N);

   Ada.Text_IO.Put_Line ("El número de Fibonacci para " & Integer'Image (N) & " es:");
   Ada.Text_IO.Put_Line (Integer'Image (Fibonacci_Rec (N)));
end Fibonacci;
```

Este código calcula el n-ésimo número de Fibonacci usando recursión.

* El paquete `Ada.Text_IO` proporciona funciones para la entrada y salida de texto.
* El paquete `Ada.Integer_Text_IO` proporciona funciones para la entrada y salida de enteros.
* El paquete `Ada.Command_Line` proporciona funciones para analizar los argumentos de la línea de comandos.

El procedimiento `Fibonacci` es el punto de entrada del programa.

* La función `Fibonacci_Rec` calcula el n-ésimo número de Fibonacci usando recursión.
    * Si `N` es menor o igual a 2, devuelve 1.
    * De lo contrario, devuelve la suma del (n-1)-ésimo y (n-2)-ésimo número de Fibonacci.

El programa solicita al usuario que ingrese un número positivo y luego imprime el n-ésimo número de Fibonacci.