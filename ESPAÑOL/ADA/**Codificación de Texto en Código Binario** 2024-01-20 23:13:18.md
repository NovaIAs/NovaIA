```ada
with Ada.Streams.Stream_IO, Ada.Text_IO;

procedure Codificar_Texto is
   type Codigo is array (Positive range <>) of Positive;

   procedure Codificar_Bit (Bit : in Positive; Codigo : out Codigo; Posicion : out Positive) is
      begin
         if Posicion > Codigo'Last then
            raise Index_Error;
         end if;

         if Bit > 0 then
            Codigo(Posicion) := Codigo(Posicion) * 2 + 1;
         else
            Codigo(Posicion) := Codigo(Posicion) * 2;
         end if;

         Posicion := Posicion + 1;
      end Codificar_Bit;

   procedure Codificar_Caracter (Caracter : in Character; Codigo : out Codigo; Posicion : out Positive) is
      begin
         for I in Caracter'Range loop
            Codificar_Bit (Caracter'Pos (I), Codigo, Posicion);
         end loop;
      end Codificar_Caracter;

   procedure Codificar_Cadena (Cadena : in String; Codigo : out Codigo; Posicion : out Positive) is
      begin
         for I in Cadena'Range loop
            Codificar_Caracter (Cadena(I), Codigo, Posicion);
         end loop;
      end Codificar_Cadena;

   procedure Mostrar_Codigo (Codigo : Codigo) is
      begin
         for I in Codigo'Range loop
            Ada.Text_IO.Put (Integer'Image (Codigo(I)));
         end loop;
         Ada.Text_IO.New_Line;
      end Mostrar_Codigo;

begin
   declare
      Codigo : Codigo := (others => 0);
      Posicion : Positive := 1;
   begin
      Codificar_Cadena ("Hola, mundo!", Codigo, Posicion);
      Mostrar_Codigo (Codigo);
   end;
end Codificar_Texto;
```

Este código es un codificador de texto sencillo que toma una cadena de texto y la codifica en un código binario. El código binario se almacena en una matriz de enteros.

El código está organizado en tres procedimientos:

* `Codificar_Bit`: Este procedimiento codifica un solo bit en la matriz de enteros.
* `Codificar_Caracter`: Este procedimiento codifica un solo carácter en la matriz de enteros.
* `Codificar_Cadena`: Este procedimiento codifica una cadena de caracteres en la matriz de enteros.

El procedimiento `Mostrar_Codigo` se utiliza para mostrar la matriz de enteros en la consola.

El código principal crea una matriz de enteros y luego llama a los procedimientos `Codificar_Cadena` y `Mostrar_Codigo` para codificar y mostrar la cadena de texto "Hola, mundo!".