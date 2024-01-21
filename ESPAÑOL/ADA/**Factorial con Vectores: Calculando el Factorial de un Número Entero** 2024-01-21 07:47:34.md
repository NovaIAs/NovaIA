```ada
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Factorial is
   type Vector_Of_Integer is array (Positive range <>) of Integer;

   function factorial_ (N : Integer) return Vector_Of_Integer is (
      Result : Vector_Of_Integer(0 .. N)          := (others => 0);
      I : Positive                                 := 0
   )
   begin
      Result(1) := 1;
      for I in 2 .. N loop
         for J in reverse Vector_Of_Integer'Range(1 .. I) loop
            Result(J) := Result(J) * I;
         end loop;
         for J in reverse Vector_Of_Integer'Range(1 .. I) loop
            if Result(J) >= 10 then
               Result(J-1) := Result(J-1) + Result(J) / 10;
               Result(J)    := Result(J) mod 10;
            end if;
         end loop;
      end loop;
      return Result;
   end factorial_;

   N : Integer;
   Result : Vector_Of_Integer;
begin
   Put("Ingrese un número entero no negativo: ");
   Get(N);
   New_Line;
   Result := factorial_(N);
   for J in Result'Range loop
      Put(Result(J), Width => 1);
   end loop;
   New_Line;
end Factorial;
```

Explicación:

* El programa calcula el factorial de un número entero no negativo.
* Se declara un tipo de vector de enteros llamado `Vector_Of_Integer`.
* Se declara una función llamada `factorial_` que toma un número entero `N` como parámetro y devuelve un vector de enteros.
* La función `factorial_` calcula el factorial de `N`.
* Se declara una variable `N` de tipo entero.
* Se declara una variable `Result` de tipo `Vector_Of_Integer`.
* Se pide al usuario que ingrese un número entero no negativo.
* Se llama a la función `factorial_` con el valor de `N` ingresado por el usuario y se asigna el resultado a la variable `Result`.
* Se imprimen los elementos del vector `Result`.