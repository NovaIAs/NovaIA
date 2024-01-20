```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Complejo is

   type Lista_Palabras is array (Positive range <>) of String;

   Nombres : Lista_Palabras := ("Juan", "María", "Pedro", "Ana", "José");

   Nombres_Ordenados : Lista_Palabras := ("", "", "", "", "");

   Indice_Nombre   : Positive range <>;
   Indice_Ordenado : Positive range <>;

begin
   for Indice_Nombre in Nombres'Range loop
      for Indice_Ordenado in Nombres_Ordenados'Range loop
         if Nombres(Indice_Nombre) <= Nombres_Ordenados(Indice_Ordenado) then
            Nombres_Ordenados(Indice_Ordenado) := Nombres(Indice_Nombre);
            exit;
         end if;

         if Indice_Ordenado = Nombres_Ordenados'Last then
            Nombres_Ordenados(Indice_Ordenado + 1) := Nombres(Indice_Nombre);
            exit;
         end if;
      end loop;
   end loop;

   for Indice_Ordenado in Nombres_Ordenados'Range loop
      Put_Line (Nombres_Ordenados(Indice_Ordenado));
   end loop;
end Complejo;
```

Explicación del código:

1. El tipo `Lista_Palabras` se define como una matriz de cadenas de caracteres.

2. La variable `Nombres` se inicializa con una lista de nombres de personas.

3. La variable `Nombres_Ordenados` se inicializa con una matriz de cadenas de caracteres vacía.

4. Se itera sobre la lista de nombres y, para cada nombre, se insertan en orden alfabético en la matriz `Nombres_Ordenados`.

5. Se itera sobre la matriz `Nombres_Ordenados` y se imprimen los nombres ordenados en la consola.