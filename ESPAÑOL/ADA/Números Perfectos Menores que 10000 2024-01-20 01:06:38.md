```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Numero_Perfecto is
   type Entero is range 1..32767;

   function Suma_Factores(N: Entero) return Entero is
      procedure Sumar(F: Entero);
      begin
         Sum := Sum + F;
      end Sumar;
   begin
      Sum := 0;
      for F in 1 .. N-1 loop
         if N mod F = 0 then
            Sumar(F);
         end if;
      end loop;
      return Sum;
   end Suma_Factores;

   function Es_Perfecto(N: Entero) return Boolean is
   begin
      return N = Suma_Factores(N);
   end Es_Perfecto;

begin
   Put("Los números perfectos menores que 10000 son:");
   for N in 1 .. 9999 loop
      if Es_Perfecto(N) then
         Put(Image(N));
         New_Line;
      end if;
   end loop;
end Numero_Perfecto;
```

Explicación:

* El procedimiento `Numero_Perfecto` es el punto de entrada del programa.

* El tipo `Entero` se define como un rango de valores enteros de 1 a 32767.

* La función `Suma_Factores` calcula la suma de los factores propios de un número entero `N`.

* La función `Es_Perfecto` comprueba si un número entero `N` es perfecto, es decir, si la suma de sus factores propios es igual a `N`.

* El programa utiliza un bucle `for` para iterar sobre los números enteros menores que 10000 y llama a la función `Es_Perfecto` para cada uno de ellos.

* Si la función `Es_Perfecto` devuelve `True`, el programa imprime el número perfecto en la consola.