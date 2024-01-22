```ada
with Ada.Text_IO, Ada.Strings.Maps, Ada.Strings.Text_Buffers, Ada.Strings.Unbounded;

procedure Mapa_Caracteres is

   type Caracteres is array (Character range '<' .. '>') of Integer;
   type Mapa_Caracteres is array (String range 1 .. 256) of Caracteres;
   type Caracter_Posicion is record
      Caracter : Character;
      Posicion : String;
      end record;

   Mapa_Car : Mapa_Caracteres;

begin

   for Car in Character'Range loop
      Mapa_Car (Strings.To_String (Car)) (Integer'Pos (Car)) := 1;
   end loop;

   for Palabra in Ada.Strings.Unbounded.Strings (Ada.Text_IO.Standard_Input) loop
      for Pos in 0 .. Palabra'Length loop
         Mapa_Car (Palabra) (Pos + 1) := Pos + 1;
      end loop;
   end loop;

   for Palabra in Ada.Strings.Unbounded.Strings (Ada.Text_IO.Standard_Input) loop
      for Pos in 0 .. Palabra'Length loop
         Put (Ada.Strings.Maps.Map_Index (Mapa_Car (Palabra), Pos + 1));
         Put (' ');
      end loop;
      New_Line;
   end loop;

end Mapa_Caracteres;
```

Este código crea un mapa de caracteres que asocia cada carácter con su posición en una palabra. El mapa se utiliza para imprimir la posición de cada carácter en una palabra.

El código utiliza el tipo `Character` para representar cada carácter. El tipo `String` se utiliza para representar cada palabra. El tipo `Caracter_Posicion` se utiliza para representar cada carácter junto con su posición en una palabra.

El mapa se crea utilizando el constructor por extensión del tipo `Mapa_Caracteres`. El bucle `for` recorre cada carácter en el rango de caracteres ASCII y establece su posición en el mapa a 1.

El bucle `for` siguiente recorre cada palabra en la entrada estándar y establece la posición de cada carácter en la palabra en el mapa.

El bucle `for` final recorre cada palabra en la entrada estándar e imprime la posición de cada carácter en la palabra utilizando el mapa.

El código utiliza el procedimiento `Ada.Strings.Maps.Map_Index` para obtener la posición de un carácter en una palabra. El procedimiento `Ada.Text_IO.New_Line` se utiliza para imprimir una nueva línea.