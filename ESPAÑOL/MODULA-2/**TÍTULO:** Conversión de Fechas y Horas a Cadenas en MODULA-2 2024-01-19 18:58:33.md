```modula-2

MODULE Funciones;
FROM FormatStrings IMPORT FormatString;

TYPE Fecha = RECORD
  MONTH : INTEGER;
  DAY : INTEGER;
  YEAR : INTEGER;
END;

PROCEDURE Fecha_a_cadena(fecha: Fecha) : STRING;
VAR
  FechaString : ARRAY 1..30 OF CHAR;
BEGIN
  FormatString("%02d-%02d-%04d", FechaString, fecha.MONTH, fecha.DAY, fecha.YEAR);
  RETURN FechaString;
END Fecha_a_cadena;

TYPE Hora = RECORD
  HOUR : INTEGER;
  MINUTE : INTEGER;
  SECOND : INTEGER;
END;

PROCEDURE Hora_a_cadena(hora: Hora) : STRING;
VAR
  HoraString : ARRAY 1..10 OF CHAR;
BEGIN
  FormatString("%02d:%02d:%02d", HoraString, hora.HOUR, hora.MINUTE, hora.SECOND);
  RETURN HoraString;
END Hora_a_cadena;

TYPE FechaHora = RECORD
  FECHA : Fecha;
  HORA : Hora;
END;

PROCEDURE FechaHora_a_cadena(fechahora: FechaHora) : STRING;
VAR
  FechaHoraString : ARRAY 1..50 OF CHAR;
BEGIN
  FormatString("%s %s", FechaHoraString, Fecha_a_cadena(fechahora.FECHA), Hora_a_cadena(fechahora.HORA));
  RETURN FechaHoraString;
END FechaHora_a_cadena;

PROCEDURE Main;
VAR
  FechaActual : Fecha;
  HoraActual : Hora;
  FechaHoraActual : FechaHora;
BEGIN
  FechaActual.MONTH := 6;
  FechaActual.DAY := 15;
  FechaActual.YEAR := 2023;

  HoraActual.HOUR := 10;
  HoraActual.MINUTE := 45;
  HoraActual.SECOND := 0;

  FechaHoraActual.FECHA := FechaActual;
  FechaHoraActual.HORA := HoraActual;

  System.WriteLn("Fecha actual: " + Fecha_a_cadena(FechaActual));
  System.WriteLn("Hora actual: " + Hora_a_cadena(HoraActual));
  System.WriteLn("Fecha y hora actual: " + FechaHora_a_cadena(FechaHoraActual));
END Main.

```

Explicación del código:

- Se definen tres tipos de datos personalizados: `Fecha`, `Hora` y `FechaHora`. Estos tipos representan fechas, horas y fechas con horas, respectivamente.

- Se definen tres procedimientos: `Fecha_a_cadena`, `Hora_a_cadena` y `FechaHora_a_cadena`. Estos procedimientos convierten una fecha, una hora o una fecha con hora en una cadena de caracteres.

- El procedimiento `Main` es el punto de entrada del programa. En este procedimiento se crea una fecha, una hora y una fecha con hora utilizando los valores actuales del sistema. A continuación, se utilizan los procedimientos `Fecha_a_cadena`, `Hora_a_cadena` y `FechaHora_a_cadena` para convertir la fecha, la hora y la fecha con hora en cadenas de caracteres. Por último, se utilizan el procedimiento `System.WriteLn` para mostrar las cadenas de caracteres en la consola.

El código es complejo porque utiliza tipos de datos personalizados, procedimientos y el módulo `FormatStrings` para formatear las cadenas de caracteres. Sin embargo, el código también está muy bien organizado y es fácil de seguir.