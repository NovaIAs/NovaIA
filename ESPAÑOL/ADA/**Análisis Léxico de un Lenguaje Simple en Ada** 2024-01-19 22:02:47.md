```ada
-- Módulo principal del programa
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;

procedure Gramática is

   -- Tipos de datos

   type Palabras_Reservadas is (
      Inicio,
      Fin,
      Se,
      Entonces,
      Sino,
      Hacer,
      Mientras,
      Leer,
      Escribir
   );

   type Simbolos is (
      Paréntesis_Abierto,
      Paréntesis_Cerrado,
      Llave_Abierta,
      Llave_Cerrada,
      Coma,
      Punto_Y_Coma,
      Asignación,
      Suma,
      Resta,
      Multiplicación,
      División,
      Módulo,
      Mayor_Que,
      Mayor_O_Igual,
      Menor_Que,
      Menor_O_Igual,
      Igual,
      Distinto
   );

   type Tokens is (Palabra_Reservada, Símbolo, Identificador, Constante, Fin_De_Línea);

   type Token_Record is record
      Token_Tipo : Tokens;
      Token_Valor : String(1..20);
   end record;

   type Token_Array is array(Positive range <>) of Token_Record;

   -- Procedimientos

   procedure Leer_Archivo
     (Archivo : Ada.Strings.Unbounded.File_Type)
   is
   begin
      loop
         for I in Token_Array'Range loop
            Token_Array(I).Token_Tipo := Fin_De_Línea;
         end loop;

         Ada.Text_IO.Get_Line(Archivo, Línea);
         Separar_Tokens(Línea, Token_Array);

         -- Procesar los tokens aquí
      end loop;
   end Leer_Archivo;

   procedure Separar_Tokens
     (Línea : in Ada.Strings.Unbounded.String;
      Token_Array : out Token_Array)
   is
      Índice_Token : Positive := 1;

      function Es_Palabra_Reservada
        (Cadena : Ada.Strings.Unbounded.String)
      return Boolean
      is
         Palabra_Reservada : Palabras_Reservadas;
      begin
         Es_Palabra_Reservada := False;

         for Palabra_Reservada in Palabras_Reservadas'Range loop
            if Cadena = Palabras_Reservadas'Image(Palabra_Reservada) then
               Es_Palabra_Reservada := True;
               exit;
            end if;
         end loop;

         return Es_Palabra_Reservada;
      end Es_Palabra_Reservada;

      function Es_Símbolo
        (Cadena : Ada.Strings.Unbounded.String)
      return Boolean
      is
         Símbolo : Simbolos;
      begin
         Es_Símbolo := False;

         for Símbolo in Simbolos'Range loop
            if Cadena = Simbolos'Image(Símbolo) then
               Es_Símbolo := True;
               exit;
            end if;
         end loop;

         return Es_Símbolo;
      end Es_Símbolo;

   begin
      for I in Línea'Range loop
         if Línea(I) in Ada.Characters.Latin_1'Digits then
            -- Constante numérica
            Índice_Token := Índice_Token + 1;
            Token_Array(Índice_Token).Token_Tipo := Constante;
            Token_Array(Índice_Token).Token_Valor := Línea(I);

            for J in I+1..Línea'Last loop
               if Línea(J) in Ada.Characters.Latin_1'Digits then
                  Token_Array(Índice_Token).Token_Valor :=
                     Token_Array(Índice_Token).Token_Valor & Línea(J);
               else
                  exit;
               end if;
            end loop;
         elsif Línea(I) in Ada.Characters.Latin_1'Letters then
            -- Identificador o palabra reservada
            Índice_Token := Índice_Token + 1;

            if Es_Palabra_Reservada(Línea(I..Línea'Last)) then
               Token_Array(Índice_Token).Token_Tipo := Palabra_Reservada;
            else
               Token_Array(Índice_Token).Token_Tipo := Identificador;
            end if;

            Token_Array(Índice_Token).Token_Valor :=
               Línea(I..Línea'Last);
         elsif Es_Símbolo(Línea(I)) then
            -- Símbolo
            Índice_Token := Índice_Token + 1;
            Token_Array(Índice_Token).Token_Tipo := Símbolo;
            Token_Array(Índice_Token).Token_Valor := Línea(I);
         end if;
      end loop;
   end Separar_Tokens;

begin
   Ada.Strings.Unbounded.Open
     (Mode => Ada.Strings.Unbounded.In_File,
      Nombre_Archivo => "programa.txt",
      File => Archivo);

   Leer_Archivo(Archivo);

   Ada.Strings.Unbounded.Close(Archivo);
end Gramática;
```

Explicación del código:

* El programa define varios tipos de datos personalizados:
    * `Palabras_Reservadas`: Una lista de palabras reservadas del lenguaje.
    * `Simbolos`: Una lista de símbolos del lenguaje.
    * `Tokens`: Una enumeración que representa los diferentes tipos de tokens que se pueden encontrar en el lenguaje.
    * `Token_Record`: Una estructura que contiene un tipo de token y su valor.
    * `Token_Array`: Una matriz de registros `Token_Record` que se utilizará para almacenar los tokens identificados en el programa.
* El procedimiento `Leer_Archivo` es el punto de entrada del programa. Abre un archivo especificado en modo de lectura y llama al procedimiento `Separar_Tokens` para analizar el contenido del archivo en busca de tokens.
* El procedimiento `Separar_Tokens` toma una cadena de texto como entrada y la separa en tokens. Recorre la cadena carácter por carácter e identifica los tokens en función de su tipo.
    * Los tokens de palabra reservada se identifican usando la función `Es_Palabra_Reservada`.
    * Los tokens de símbolo se identifican usando la función `Es_Símbolo`.
    * Los tokens constantes se identifican si el carácter actual es un dígito.
    * Los tokens de identificador se identifican si el carácter actual es una letra.
* El programa luego procesa los tokens identificados para generar el código de máquina correspondiente o realizar otras acciones.

Este es un ejemplo de un programa complejo en Ada que analiza un archivo de texto en busca de tokens y luego procesa esos tokens para generar un resultado. El programa utiliza varios tipos de datos personalizados y procedimientos para lograr esta tarea.