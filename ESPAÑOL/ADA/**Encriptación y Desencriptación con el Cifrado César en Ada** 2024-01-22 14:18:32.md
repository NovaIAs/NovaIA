```ada
with Ada.Integer_Text_IO;
with Ada.Strings.Utf_Encoding;
with Ada.Strings.Unbounded;

procedure Complejo is

   Alphabet : constant String := "ABCDEFGHIKLMNOPQRSTVXYZ";

   function Rotar_Letra(Letra : Character; Cant_Letras : Positive) return Character is (
      Posicion_Letra : Positive := Alphabet'Index(Letra);
      Posicion_Nueva := (Posicion_Letra + Cant_Letras - 1) mod Alphabet'Length + 1;
      Letra_Nueva : Character := Alphabet(Posicion_Nueva)
   );

   function Encriptar_Cadena(Cadena : String; Cant_Letras : Positive) return String is (
      Cadena_Encriptada : String := "";
   begin
      for I in 1 .. Cadena'Length loop
         Cadena_Encriptada := Cadena_Encriptada & Rotar_Letra(Cadena(I), Cant_Letras);
      end loop;
      return Cadena_Encriptada;
   );

   function Desencriptar_Cadena(Cadena : String; Cant_Letras : Positive) return String is (
      Cadena_Desencriptada : String := "";
   begin
      for I in 1 .. Cadena'Length loop
         Cadena_Desencriptada := Cadena_Desencriptada & Rotar_Letra(Cadena(I), -Cant_Letras);
      end loop;
      return Cadena_Desencriptada;
   );

   Cadena_Original : String := "Hola Mundo!";
   Cant_Letras : constant Positive := 3;

begin
   Ada.Integer_Text_IO.Put("Cadena original: ");
   Ada.Strings.Utf_Encoding.Put_Line(Cadena_Original);

   Ada.Integer_Text_IO.Put("Cantidad de letras a rotar: ");
   Ada.Integer_Text_IO.Get(Cant_Letras);

   Ada.Integer_Text_IO.New_Line;

   Cadena_Encriptada : String := Encriptar_Cadena(Cadena_Original, Cant_Letras);
   Ada.Integer_Text_IO.Put("Cadena encriptada: ");
   Ada.Strings.Utf_Encoding.Put_Line(Cadena_Encriptada);

   Cadena_Desencriptada : String := Desencriptar_Cadena(Cadena_Encriptada, Cant_Letras);
   Ada.Integer_Text_IO.Put("Cadena desencriptada: ");
   Ada.Strings.Utf_Encoding.Put_Line(Cadena_Desencriptada);
end Complejo;
```

Este programa es un ejemplo de cómo implementar un cifrado César en Ada. El cifrado César es un método de cifrado simple que consiste en rotar cada letra del mensaje un determinado número de posiciones en el alfabeto. El programa primero pide al usuario la cadena de texto que quiere encriptar y la cantidad de letras que quiere rotar. Luego, utiliza las funciones `Encriptar_Cadena` y `Desencriptar_Cadena` para encriptar y desencriptar la cadena, respectivamente.

El código utiliza la unidad `Ada.Strings.Utf_Encoding` para manejar cadenas de texto en formato UTF-8. La unidad `Ada.Strings.Unbounded` se utiliza para declarar cadenas de texto de longitud ilimitada.

La función `Rotar_Letra` rota una letra un determinado número de posiciones en el alfabeto. La función `Encriptar_Cadena` utiliza la función `Rotar_Letra` para encriptar una cadena de texto. La función `Desencriptar_Cadena` utiliza la función `Rotar_Letra` para desencriptar una cadena de texto.

El programa es un ejemplo de cómo implementar un cifrado sencillo en Ada. El código es complejo porque utiliza varias unidades y funciones diferentes.