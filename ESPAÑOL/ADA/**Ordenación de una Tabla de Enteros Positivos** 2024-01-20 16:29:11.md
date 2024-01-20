```ada
with Ada.Text_IO;
with Ada.Strings.Generic_Array_Sorting;
with Ada.Strings.Fixed;

procedure Code_Complejo is

   type Mi_Array is array (Positive range <>) of Positive;

   procedure Escribe_En_Fichero (Texto : String; Fichero : Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line (Fichero, Texto);
   end Escribe_En_Fichero;

   procedure Escribe_Array (Array : Mi_Array; Fichero : Ada.Text_IO.File_Type) is
   begin
      for I in Array'Range loop
         Escribe_En_Fichero (Integer'Image (Array (I)), Fichero);
      end loop;
   end Escribe_Array;

   procedure Ordena_Array (Array : in out Mi_Array) is
      use Ada.Strings.Generic_Array_Sorting;

      procedure Ordenar (Array : in out Mi_Array) is
      begin
         Array_Sort (Array, Array'First, Array'Last, <);
      end Ordenar;

      Tipo_Array : access procedure (Mi_Array) is Ordenar;

      function Clave (Elemento : Mi_Array'Element_Type) return Positive is
      begin
         return Elemento;
      end Clave;

      Auxiliar : Mi_Array (Array'First .. Array'Last);
   begin
      for I in Array'Range loop
         Auxiliar (I) := Array (I);
      end loop;

      Tipo_Array (Auxiliar);

      for I in Array'Range loop
         Array (I) := Auxiliar (I);
      end loop;
   end Ordena_Array;

begin
   Fichero_Salida : Ada.Text_IO.File_Type;

   Mi_Array := (2, 1, 4, 3, 5);

   Ada.Text_IO.Open (File => Fichero_Salida, Mode => Ada.Text_IO.Out_File,
                      Name => "array.txt");

   Escribe_En_Fichero ("Array original:", Fichero_Salida);
   Escribe_Array (Mi_Array, Fichero_Salida);

   Ordena_Array (Mi_Array);

   Escribe_En_Fichero ("Array ordenado:", Fichero_Salida);
   Escribe_Array (Mi_Array, Fichero_Salida);

   Ada.Text_IO.Close (Fichero_Salida);
end Code_Complejo;
```

Explicación:

* El código define un tipo personalizado `Mi_Array` que es una tabla de enteros positivos.
* El procedimiento `Escribe_En_Fichero` escribe una cadena de texto en un fichero.
* El procedimiento `Escribe_Array` escribe los elementos de una tabla en un fichero.
* El procedimiento `Ordena_Array` ordena los elementos de una tabla en orden ascendente.
* El procedimiento `Clave` se utiliza para comparar los elementos de la tabla en el procedimiento `Ordena_Array`.
* La variable `Auxiliar` se utiliza para almacenar una copia de la tabla original antes de ordenarla.
* El procedimiento `Ordenar` ordena los elementos de la tabla utilizando el algoritmo de ordenación por selección.
* El tipo de acceso `Tipo_Array` se utiliza para almacenar una referencia al procedimiento `Ordenar`.
* La función `Clave` devuelve el valor del elemento de la tabla que se está comparando.
* El procedimiento `Ordena_Array` ordena los elementos de la tabla llamando al procedimiento `Tipo_Array` con la tabla como argumento.
* El procedimiento `Code_Complejo` crea una tabla de enteros positivos, la ordena y escribe los elementos de la tabla ordenada en un fichero.