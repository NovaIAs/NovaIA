```ada
with Ada.Strings.Text_IO;

procedure Ordenar_Nombres is
   type Nombre_Tipo is array (1 .. 50) of Character;
   type Nombres_Tipo is array (1 .. 10) of Nombre_Tipo;

   Nombres : Nombres_Tipo := (Nombre_Tipo'("Juan"),    Nombre_Tipo'("María"),
                             Nombre_Tipo'("Pedro"),    Nombre_Tipo'("Ana"),
                             Nombre_Tipo'("José"),     Nombre_Tipo'("Isabel"),
                             Nombre_Tipo'("Luis"),     Nombre_Tipo'("Carmen"),
                             Nombre_Tipo'("Manuel"),   Nombre_Tipo'("Rosa"));
   begin
      -- Ordenar los nombres en orden alfabético

      for I in 1 .. Nombres'Length loop
         for J in I + 1 .. Nombres'Length loop
            if Nombres(I) > Nombres(J) then
               Nombres(I), Nombres(J) := Nombres(J), Nombres(I);
            end if;
         end loop;
      end loop;

      -- Mostrar los nombres ordenados

      for I in 1 .. Nombres'Length loop
         Ada.Strings.Text_IO.Put(Nombres(I));
         Ada.Strings.Text_IO.New_Line;
      end loop;
   end Ordenar_Nombres;
```

Este programa ordena un array de nombres en orden alfabético.

El programa comienza declarando los tipos `Nombre_Tipo` y `Nombres_Tipo`. `Nombre_Tipo` es un tipo de array de 50 caracteres, y `Nombres_Tipo` es un tipo de array de 10 `Nombre_Tipo`.

A continuación, se declara una variable `Nombres` de tipo `Nombres_Tipo`. Esta variable contiene los nombres que se van a ordenar.

El programa entonces entra en un bucle que ordena los nombres en `Nombres`. El bucle itera a través de los nombres en `Nombres`, y para cada nombre, compara ese nombre con el nombre siguiente. Si el nombre actual es mayor que el nombre siguiente, entonces los dos nombres se intercambian.

Una vez que el bucle termina, los nombres en `Nombres` están ordenados en orden alfabético.

Finalmente, el programa muestra los nombres ordenados en la consola.

Este programa demuestra el uso de tipos de array, bucles y la instrucción `if`. También demuestra cómo se puede utilizar Ada para manipular cadenas de caracteres.