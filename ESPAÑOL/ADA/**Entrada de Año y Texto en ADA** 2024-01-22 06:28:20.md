```ada
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Cadenas is

   type Año_Cadenas is new String(1..15);     -- Tipo de cadena para el año
   type Texto_Cadenas is new String(0..63);   -- Tipo de cadena para el texto

   Mensajes : constant array(1..6) of Texto_Cadenas := (  -- Mensajes para mostrar
      "Introduce el año: ",
      "Introduce el texto: ",
      "El texto introducido es: ",
      "Texto no válido. Longitud mínima: 1 carácter y máxima: 64 caracteres.",
      "El año introducido es: ",
      "Año no válido. Rango de años admitido: 1900 a 2099."
   );

   Año : Año_Cadenas;    -- Variable para guardar el año
   Texto : Texto_Cadenas;  -- Variable para guardar el texto

begin
   Put(Mensajes(1));       -- Mostrar mensaje de solicitud del año
   Get(Año);                -- Leer el año
   if Año(1) /= '1' then
      Put_Line(Mensajes(5)); -- Año no válido
   elsif Año(2) > '9' then
      Put_Line(Mensajes(5)); -- Año no válido
   elsif Año(3) > '9' then
      Put_Line(Mensajes(5)); -- Año no válido
   elsif Año(4) < '0' or Año(4) > '9' then
      Put_Line(Mensajes(5)); -- Año no válido
   else
      Put(Mensajes(2));       -- Mostrar mensaje de solicitud del texto
      Get_Line(Texto);         -- Leer el texto
      if Texto'Length < 1 then
         Put_Line(Mensajes(3)); -- Texto no válido
      elsif Texto'Length > 64 then
         Put_Line(Mensajes(3)); -- Texto no válido
      else
         Put(Mensajes(3));         -- Mostrar mensaje de confirmación del texto
         Put_Line(Texto);          -- Mostrar el texto
         Put(Mensajes(4));         -- Mostrar mensaje de confirmación del año
         Put(Año);                  -- Mostrar el año
      end if;
   end if;
end Cadenas;
```

Explicación:

* El código ADA crea un programa que pide al usuario que introduzca un año y un texto. Luego, comprueba si el año y el texto son válidos según las siguientes reglas:
    * El año debe estar en el rango 1900-2099.
    * El texto debe tener una longitud entre 1 y 64 caracteres.
* Si el año o el texto no son válidos, el programa muestra un mensaje de error.
* Si el año y el texto son válidos, el programa muestra el texto y el año.

El código está dividido en las siguientes partes:

* **Tipos de datos:**
    * Se definen dos tipos de datos personalizados: `Año_Cadenas` y `Texto_Cadenas`. Estos tipos de datos se utilizan para representar el año y el texto introducido por el usuario.
* **Constantes:**
    * Se define una constante llamada `Mensajes` que contiene los mensajes que se muestran al usuario.
* **Variables:**
    * Se definen dos variables: `Año` y `Texto`. Estas variables se utilizan para almacenar el año y el texto introducido por el usuario.
* **Función principal:**
    * La función principal `Cadenas` es el punto de entrada del programa.
    * Dentro de la función principal, se realizan las siguientes acciones:
        * Se muestra un mensaje al usuario solicitando el año.
        * Se lee el año introducido por el usuario.
        * Se comprueba si el año es válido.
            * Si el año no es válido, se muestra un mensaje de error.
            * Si el año es válido, se continúa con el siguiente paso.
        * Se muestra un mensaje al usuario solicitando el texto.
        * Se lee el texto introducido por el usuario.
        * Se comprueba si el texto es válido.
            * Si el texto no es válido, se muestra un mensaje de error.
            * Si el texto es válido, se continúa con el siguiente paso.
        * Se muestra un mensaje al usuario confirmando el texto.
        * Se muestra el texto.
        * Se muestra un mensaje al usuario confirmando el año.
        * Se muestra el año.