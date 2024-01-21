```modula-2
MODULE ProgramaPrincipal;
FROM Keyboard IMPORT WriteString, ReadChar;
FROM Terminal IMPORT WriteStringLn;
FROM TextIO IMPORT ReadLine, WriteStringLn;
FROM Strings IMPORT ToUpper;

VAR
  Nombre: ARRAY 20 OF CHAR;
  Edad: INTEGER;
  Sexo: CHAR;

PROCEDURE MostrarMensaje(Mensaje: STRING);
VAR
  Tecla: CHAR;
BEGIN
  WriteString(Mensaje);
  ReadChar(Tecla);
END MostrarMensaje;

PROCEDURE LeerNombre(VAR Nombre: ARRAY OF CHAR);
VAR
  i: INTEGER;
BEGIN
  WriteString("Ingrese su nombre: ");
  ReadLine(Nombre);
  FOR i := 1 TO HIGH(Nombre) DO
    Nombre[i] := ToUpper(Nombre[i]);
  END;
END LeerNombre;

PROCEDURE LeerEdad(VAR Edad: INTEGER);
VAR
  Cadena: ARRAY 80 OF CHAR;
BEGIN
  WriteString("Ingrese su edad: ");
  ReadLine(Cadena);
  Edad := INTEGER(Cadena);
END LeerEdad;

PROCEDURE LeerSexo(VAR Sexo: CHAR);
BEGIN
  WriteString("Ingrese su sexo (M/F): ");
  Sexo := ReadChar;
END LeerSexo;

PROCEDURE MostrarDatos(Nombre: ARRAY OF CHAR; Edad: INTEGER; Sexo: CHAR);
BEGIN
  WriteStringLn("Nombre: " + Nombre);
  WriteStringLn("Edad: " + INTEGER'IMAGE(Edad));
  WriteStringLn("Sexo: " + Sexo);
END MostrarDatos;

BEGIN
  LeerNombre(Nombre);
  LeerEdad(Edad);
  LeerSexo(Sexo);
  MostrarMensaje("Datos ingresados. Presione cualquier tecla para continuar...");
  MostrarDatos(Nombre, Edad, Sexo);
END ProgramaPrincipal.
```

Explicación del código:

* El código está escrito en el lenguaje de programación Modula-2.
* El programa principal es `ProgramaPrincipal`.
* El programa utiliza los siguientes módulos:
    * `Keyboard`: Módulo para manejar la entrada y salida del teclado.
    * `Terminal`: Módulo para manejar la salida en la terminal.
    * `TextIO`: Módulo para manejar la entrada y salida de texto.
    * `Strings`: Módulo para manejar cadenas de caracteres.
* El programa declara las siguientes variables:
    * `Nombre`: Arreglo de caracteres de 20 posiciones para almacenar el nombre del usuario.
    * `Edad`: Variable entera para almacenar la edad del usuario.
    * `Sexo`: Variable carácter para almacenar el sexo del usuario.
* El programa define los siguientes procedimientos:
    * `MostrarMensaje`: Muestra un mensaje en la pantalla y espera que el usuario presione una tecla para continuar.
    * `LeerNombre`: Lee el nombre del usuario desde el teclado y lo almacena en el arreglo `Nombre`.
    * `LeerEdad`: Lee la edad del usuario desde el teclado y la almacena en la variable `Edad`.
    * `LeerSexo`: Lee el sexo del usuario desde el teclado y lo almacena en la variable `Sexo`.
    * `MostrarDatos`: Muestra los datos del usuario en la pantalla.
* El programa principal llama a los procedimientos `LeerNombre`, `LeerEdad` y `LeerSexo` para leer los datos del usuario.
* El programa principal llama al procedimiento `MostrarMensaje` para mostrar un mensaje en la pantalla y esperar que el usuario presione una tecla para continuar.
* El programa principal llama al procedimiento `MostrarDatos` para mostrar los datos del usuario en la pantalla.