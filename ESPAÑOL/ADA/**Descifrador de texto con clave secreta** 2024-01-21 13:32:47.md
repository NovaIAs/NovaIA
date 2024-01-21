```ada
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Descifrar is
-- Declaramos las variables e inicializamos algunos valores
   Clave: String(1..20) := "Frase secreta";
   Texto: String(1..80);
   TextoDescifrado: Unbounded_String;
   IndiceClave: Integer := 1;
   IndiceTexto: Integer := 1;
   LetraClave: Character;
   LetraTexto: Character;

begin
   -- Obtenemos el texto a descifrar desde la línea de comandos
   if Argument_Count > 0 then
      Texto := Argument(1);
   else
      Put_Line("No se ha proporcionado ningún texto a descifrar.");
      return;
   end if;

   -- Desciframos el texto carácter a carácter.
   while IndiceTexto <= Texto'Last then
      -- Obtenemos la letra de la clave y del texto
      LetraClave := Clave(IndiceClave);
      LetraTexto := Texto(IndiceTexto);

      -- Si la letra de la clave es una letra mayúscula, la convertimos a minúscula.
      if LetraClave >= 'A' and LetraClave <= 'Z' then
         LetraClave := LetraClave - 'A' + 'a';
      end if;

      -- Si la letra del texto es una letra mayúscula, la convertimos a minúscula.
      if LetraTexto >= 'A' and LetraTexto <= 'Z' then
         LetraTexto := LetraTexto - 'A' + 'a';
      end if;

      -- Desciframos la letra del texto
      LetraTexto := chr(Integer(LetraTexto) - Integer(LetraClave) + Integer('a'));

      -- Si la letra del texto se salió del rango de las letras minúsculas, la ajustamos.
      if LetraTexto < 'a' then
         LetraTexto := LetraTexto + 26;
      elsif LetraTexto > 'z' then
         LetraTexto := LetraTexto - 26;
      end if;

      -- Añadimos la letra descifrada al texto descifrado.
      TextoDescifrado.Append(LetraTexto);

      -- Incrementamos los índices de la clave y del texto.
      IndiceClave := IndiceClave + 1;
      if IndiceClave > Clave'Last then
         IndiceClave := 1;
      end if;
      IndiceTexto := IndiceTexto + 1;
   end loop;

   -- Mostramos el texto descifrado.
   Put_Line(TextoDescifrado);
end Descifrar;
```

Este código es un programa sencillo que se utiliza para descifrar un texto utilizando un cifrado de clave secreta. El programa toma un texto como argumento de línea de comandos y lo descifra utilizando una clave secreta. El texto descifrado se muestra en la salida estándar.

El programa funciona de la siguiente manera:

1. Primero, se declaran las variables e inicializan algunos valores.
2. Luego, se obtiene el texto a descifrar desde la línea de comandos.
3. A continuación, se descifra el texto carácter a carácter.
4. Para descifrar una letra del texto, el programa obtiene la letra de la clave y de texto.
5. Si la letra de la clave es una letra mayúscula, se convierte a minúscula.
6. Si la letra del texto es una letra mayúscula, se convierte a minúscula.
7. Después, se descifra la letra del texto.
8. Si la letra del texto se salió del rango de las letras minúsculas, se ajusta.
9. Luego, se añade la letra descifrada al texto descifrado.
10. Finalmente, se incrementan los índices de la clave y del texto.
11. Cuando se ha descifrado todo el texto, se muestra el texto descifrado en la salida estándar.