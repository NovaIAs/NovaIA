```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Ordenar_Enteros (A: in out Integer_Array) is
   N: constant Integer := A'Length;
   I, J: Integer;
   Auxiliar: Integer;
begin
   for J in 1 .. N - 1 loop
      for I in J + 1 .. N loop
         if A(I) < A(J) then
            Auxiliar := A(I);
            A(I) := A(J);
            A(J) := Auxiliar;
         end if;
      end loop;
   end loop;
end Ordenar_Enteros;

procedure Main is
   A: Integer_Array (1 .. 10);
   I: Integer;
begin
   Put_Line ("Introduzca 10 enteros:");
   for I in A'Range loop
      Get (Integer_Text_IO.Integer_IO (I));
   end loop;
   Ordenar_Enteros (A);
   Put_Line ("Los enteros ordenados son:");
   for I in A'Range loop
      Put (A(I));
      if I < A'Last then
         Put (", ");
      end if;
   end loop;
   New_Line;
end Main;
```

Este programa ordena una lista de 10 enteros introducidos por el usuario utilizando el algoritmo de la burbuja.

El programa consta de dos procedimientos:

- `Ordenar_Enteros`: Recibe un array de enteros y lo ordena en orden ascendente.
- `Main`: Es el procedimiento principal del programa. Solicita al usuario que introduzca 10 enteros, llama al procedimiento `Ordenar_Enteros` para ordenarlos y luego los muestra por pantalla.

El procedimiento `Ordenar_Enteros` utiliza el algoritmo de la burbuja para ordenar el array de enteros. El algoritmo de la burbuja consiste en comparar cada elemento del array con el siguiente y, si el primer elemento es mayor que el segundo, intercambiarlos. El algoritmo se repite hasta que no se produzcan más intercambios.

El procedimiento `Main` utiliza el procedimiento `Get` para solicitar al usuario que introduzca 10 enteros. El procedimiento `Get` lee una línea de texto de la entrada estándar y la convierte en un entero.

El procedimiento `Main` utiliza el procedimiento `Put` para mostrar el array de enteros ordenados por pantalla. El procedimiento `Put` convierte un entero en una cadena de texto y la escribe en la salida estándar.

El programa utiliza el paquete `Ada.Text_IO` para leer y escribir texto de la entrada estándar y la salida estándar. El programa también utiliza el paquete `Ada.Integer_Text_IO` para convertir enteros en cadenas de texto y viceversa.