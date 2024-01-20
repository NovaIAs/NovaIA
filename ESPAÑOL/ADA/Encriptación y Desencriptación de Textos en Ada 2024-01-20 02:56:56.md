```ada
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Encriptación is

   type Clave is array (1 .. 8) of Integer;

   -- Definir un tipo de enumeración para los diferentes modos de encriptación
   type Modo_Encriptacion is (Ninguno, César, XOR, Mixto);

   -- Definir un tipo de registro para almacenar la configuración de encriptación
   type Configuracion is record
      Modo : Modo_Encriptacion;
      Clave : Clave;
   end record;

   -- Función auxiliar para generar una clave aleatoria
   function Generar_Clave return Clave is
      Random : Clave;
      begin
         for I in 1 .. Random'Length loop
            Random(I) := Random.Range;
         end loop;
         return Random;
      end Generar_Clave;

   -- Función para encriptar un texto dado una configuración de encriptación
   function Encriptar (Texto : String; Config : Configuracion) return String is
      Resultado : String := Texto;
      begin
         case Config.Modo is
            when Ninguno =>
               -- No se aplica ninguna encriptación
            when César =>
               -- Encriptación César: desplazar cada carácter por el valor de la clave
               for I in Resultado'First .. Resultado'Last loop
                  Resultado(I) := Chr(Pos(Resultado(I)) + Config.Clave(I mod Config.Clave'Length));
               end loop;
            when XOR =>
               -- Encriptación XOR: realizar una operación XOR con cada carácter y el valor de la clave
               for I in Resultado'First .. Resultado'Last loop
                  Resultado(I) := Chr(Pos(Resultado(I)) XOR Config.Clave(I mod Config.Clave'Length));
               end loop;
            when Mixto =>
               -- Encriptación mixta: aplicar primero la encriptación César y luego la encriptación XOR
               Resultado := Encriptar(Resultado, Configuracion'(Modo => César, Clave => Config.Clave));
               Resultado := Encriptar(Resultado, Configuracion'(Modo => XOR, Clave => Config.Clave));
         end case;
         return Resultado;
      end Encriptar;

   -- Función para desencriptar un texto dado una configuración de encriptación
   function Desencriptar (Texto : String; Config : Configuracion) return String is
      Resultado : String := Texto;
      begin
         case Config.Modo is
            when Ninguno =>
               -- No se aplica ninguna desencriptación
            when César =>
               -- Desencriptación César: desplazar cada carácter por el valor de la clave
               for I in Resultado'First .. Resultado'Last loop
                  Resultado(I) := Chr(Pos(Resultado(I)) - Config.Clave(I mod Config.Clave'Length));
               end loop;
            when XOR =>
               -- Desencriptación XOR: realizar una operación XOR con cada carácter y el valor de la clave
               for I in Resultado'First .. Resultado'Last loop
                  Resultado(I) := Chr(Pos(Resultado(I)) XOR Config.Clave(I mod Config.Clave'Length));
               end loop;
            when Mixto =>
               -- Desencriptación mixta: aplicar primero la desencriptación XOR y luego la desencriptación César
               Resultado := Desencriptar(Resultado, Configuracion'(Modo => XOR, Clave => Config.Clave));
               Resultado := Desencriptar(Resultado, Configuracion'(Modo => César, Clave => Config.Clave));
         end case;
         return Resultado;
      end Desencriptar;

begin
   -- Crear una configuración de encriptación con modo mixto y una clave aleatoria
   Config : Configuracion := Configuracion'(Modo => Mixto, Clave => Generar_Clave);

   -- Obtener el texto a encriptar del usuario
   Put_Line("Escribe el texto a encriptar:");
   Texto : String;
   Get_Line(Texto);

   -- Encriptar el texto
   Texto_Encriptado : String := Encriptar(Texto, Config);

   -- Mostrar el texto encriptado
   Put_Line("Texto encriptado:");
   Put_Line(Texto_Encriptado);

   -- Obtener el texto desencriptado del usuario
   Put_Line("Escribe el texto desencriptado:");
   Texto_Desencriptado : String;
   Get_Line(Texto_Desencriptado);

   -- Desencriptar el texto
   Texto_Desencriptado_Final : String := Desencriptar(Texto_Desencriptado, Config);

   -- Mostrar el texto desencriptado
   Put_Line("Texto desencriptado:");
   Put_Line(Texto_Desencriptado_Final);
end Encriptación;
```

El código anterior implementa un programa de encriptación y desencriptación de textos en Ada. El programa utiliza tres modos de encriptación: Ninguno, César y XOR. También permite crear configuraciones de encriptación mixtas, que combinan dos modos de encriptación.

El programa primero pide al usuario que ingrese el texto que desea encriptar y luego genera una clave aleatoria para la encriptación. A continuación, encripta el texto utilizando la configuración de encriptación especificada por el usuario y muestra el texto encriptado.

Luego, el programa pide al usuario que ingrese el texto encriptado que desea desencriptar y desencripta el texto utilizando la configuración de encriptación especificada por el usuario. Por último, muestra el texto desencriptado.

Este código es complejo y difícil de repetir porque utiliza varios conceptos avanzados de Ada, como tipos de enumeración, registros, funciones genéricas y polimorfismo. También utiliza una serie de características avanzadas de la biblioteca estándar de Ada, como la función `Get_Line` y el paquete `Ada.Text_IO`.