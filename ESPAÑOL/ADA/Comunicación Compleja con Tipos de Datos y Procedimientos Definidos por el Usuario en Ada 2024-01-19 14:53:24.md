```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Comunicacion_Compleja is
  type Mensaje_Avanzado is array (Natural) of Character;

  function Crear_Mensaje (Texto : String) return Mensaje_Avanzado
  is
    Mensaje : Mensaje_Avanzado (1 .. Texto'Length);
  begin
    for i in Mensaje'Range loop
      Mensaje (i) := Texto (i);
    end loop;
    return Mensaje;
  end Crear_Mensaje;

  procedure Mostrar_Mensaje (Mensaje : in Mensaje_Avanzado) is
  begin
    for i in Mensaje'Range loop
      Ada.Text_IO.Put (Mensaje (i));
    end loop;
  end Mostrar_Mensaje;

  Procedimiento_Avanzado : procedure is
    Mensaje_1 : Mensaje_Avanzado := Crear_Mensaje ("Hola, mundo!");
    Mensaje_2 : Mensaje_Avanzado := Crear_Mensaje ("El número pi es:");
    Pi : Float := 3.14159;
  begin
    Mostrar_Mensaje (Mensaje_1);
    Ada.Text_IO.New_Line;
    Mostrar_Mensaje (Mensaje_2);
    Ada.Integer_Text_IO.Put (Pi, Fore => 4, Aft => 8);
    Ada.Text_IO.New_Line;
  end Procedimiento_Avanzado;

begin
  Procedimiento_Avanzado;
end Comunicacion_Compleja;
```

Explicación:

- Este código de Ada implementa un programa que muestra un mensaje complejo (hola mundo y el valor flotante 3.14159) en la consola.

- El tipo `Mensaje_Avanzado` es una matriz de caracteres que representa un mensaje complejo.

- La función `Crear_Mensaje` toma una cadena de texto como parámetro y devuelve un `Mensaje_Avanzado`. La función convierte la cadena de texto en un `Mensaje_Avanzado` copiando cada carácter de la cadena en una posición correspondiente en el `Mensaje_Avanzado`.

- El procedimiento `Mostrar_Mensaje` toma un `Mensaje_Avanzado` como parámetro y muestra el mensaje en la consola. El procedimiento recorre el `Mensaje_Avanzado` y muestra cada carácter en la consola.

- El procedimiento `Procedimiento_Avanzado` crea dos `Mensaje_Avanzado` utilizando la función `Crear_Mensaje`. El primer `Mensaje_Avanzado` contiene el mensaje "Hola, mundo!". El segundo `Mensaje_Avanzado` contiene el mensaje "El número pi es:". El procedimiento también define una variable de tipo `Float` llamada `Pi` y le asigna el valor 3.14159.

- El procedimiento `Procedimiento_Avanzado` llama al procedimiento `Mostrar_Mensaje` para mostrar los dos `Mensaje_Avanzado`. El procedimiento también utiliza la función `Ada.Integer_Text_IO.Put` para mostrar el valor de `Pi` en la consola.

- El programa se inicia llamando al procedimiento `Comunicacion_Compleja`, que a su vez llama al procedimiento `Procedimiento_Avanzado`. El resultado del programa es que se muestran en la consola los dos mensajes complejos y el valor de `Pi`.