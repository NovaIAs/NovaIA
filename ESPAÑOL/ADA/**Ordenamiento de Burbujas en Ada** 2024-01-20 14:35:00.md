```ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Ordenamiento de Burbujas es
   A : array(1 .. 10) of Integer;
   i, j : Integer;
   aux : Integer;
begin
   A(1) := 10; A(2) := 5; A(3) := 1; A(4) := 7; A(5) := 2;
   A(6) := 4; A(7) := 3; A(8) := 9; A(9) := 8; A(10) := 6;

   -- Imprimir el arreglo antes de ordenar
   for i in A'range loop
      Put(A(i));
   end loop;
   New_Line;

   -- Ordenar el arreglo usando el algoritmo de burbuja
   for i in A'range loop
      for j in i+1 .. A'last loop
         if A(i) > A(j) then
            aux := A(i);
            A(i) := A(j);
            A(j) := aux;
         end if;
      end loop;
   end loop;

   -- Imprimir el arreglo después de ordenar
   for i in A'range loop
      Put(A(i));
   end loop;
   New_Line;
end Ordenamiento de Burbujas;
```

Explicación:

* Este programa en Ada implementa el algoritmo de ordenamiento de burbuja para ordenar un arreglo de enteros.
* El arreglo `A` se inicializa con una lista de enteros.
* El algoritmo de burbuja recorre el arreglo de enteros y compara cada elemento con el siguiente.
* Si el elemento actual es mayor que el siguiente, se intercambian sus posiciones.
* El algoritmo recorre el arreglo varias veces hasta que no se produzcan más intercambios.
* El programa imprime el arreglo antes y después de ordenar.