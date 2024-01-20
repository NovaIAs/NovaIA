```modula-2
MODULE Modulo2Ejemplo;

FROM DECLARE IMPORT Declare;
FROM PrettyPrinter IMPORT PrettyPrint;
FROM Strings IMPORT String,StringLength,StringCompare;

TYPE
  Persona = RECORD
    nombre: ARRAY 256 OF CHAR;
    edad: CARDINAL;
  END;

TYPE
  ListaPersonas = ARRAY 1000 OF Persona;

VAR
  personas: ListaPersonas;
  numPersonas: CARDINAL;

PROCEDURE Init() IS
BEGIN
  numPersonas := 0;
END Init;

PROCEDURE AddPersona(nombre: ARRAY 256 OF CHAR; edad: CARDINAL) IS
VAR
  i: CARDINAL;
BEGIN
  IF numPersonas < 1000 THEN
    FOR i TO numPersonas DO
      IF StringCompare(personas[i].nombre, nombre) = 0 THEN
        RETURN;
      END IF;
    END FOR;
    personas[numPersonas].nombre := nombre;
    personas[numPersonas].edad := edad;
    numPersonas := numPersonas + 1;
  END IF;
END AddPersona;

PROCEDURE PrintPersonas() IS
VAR
  i: CARDINAL;
BEGIN
  FOR i TO numPersonas - 1 DO
    PrettyPrint(personas[i].nombre, personas[i].edad);
  END FOR;
END PrintPersonas;

BEGIN
  Init();
  AddPersona("Juan", 20);
  AddPersona("María", 25);
  AddPersona("Pedro", 30);
  PrintPersonas();
END Modulo2Ejemplo.
```

Explicación:

* El módulo `Modulo2Ejemplo` define un tipo `Persona` que contiene un nombre y una edad, y un tipo `ListaPersonas` que es un array de 1000 elementos de tipo `Persona`.
* La variable `personas` es una instancia del tipo `ListaPersonas`, y la variable `numPersonas` es un contador del número de personas almacenadas en la lista.
* El procedimiento `Init()` inicializa el número de personas a 0.
* El procedimiento `AddPersona()` añade una nueva persona a la lista, si la persona no existe ya en la lista.
* El procedimiento `PrintPersonas()` imprime los nombres y edades de todas las personas de la lista.
* El procedimiento `PrettyPrint()` es una función auxiliar que imprime una cadena y un número entero con un formato bonito.

Este código es un ejemplo de cómo utilizar tipos de datos definidos por el usuario, arrays y procedimientos en Modula-2.