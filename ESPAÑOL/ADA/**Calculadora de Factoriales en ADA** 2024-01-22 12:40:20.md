```ada
with Ada.Text_IO;

procedure CalculaFactorial es
   -- Función que calcula el factorial de un número.
   function Factorial(N : Natural) return Natural is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial(N - 1);
      end if;
   end Factorial;

   -- Programa principal.
begin
   -- Declaración de variables.
   N : Natural;

   -- Entrada de datos.
   Ada.Text_IO.Put("Introduce un número para calcular su factorial: ");
   Ada.Text_IO.Get(N);

   -- Cálculo del factorial.
   Factorial := Factorial(N);

   -- Salida de datos.
   Ada.Text_IO.Put_Line("El factorial de " & Natural'Image(N) & " es " & Natural'Image(Factorial));
end CalculaFactorial;
```

Este código calcula el factorial de un número introducido por el usuario.

La función `Factorial` calcula el factorial de un número usando recursividad. La función se llama a sí misma de forma recursiva hasta que el número llega a 0, en cuyo caso devuelve 1. En cada llamada recursiva, la función multiplica el número por el resultado de la llamada recursiva anterior.

El programa principal declara una variable `N` para almacenar el número introducido por el usuario. Luego llama a la función `Factorial` para calcular el factorial del número y muestra el resultado en la consola.