```modula-2

MODULE DispositivosDeEntradaYSalida;
FROM InOut IMPORT WriteString, WriteCard, WriteLn;

TYPE
  Dispositivos = ARRAY [0..4] OF CHAR;
  Dispositivo = RECORD
    nombre: Dispositivos;
  END;
  DispositivosList = ARRAY [0..10] OF Dispositivo;

VAR
  dispositivos: DispositivosList;

PROCEDURE EscribaDispositivos;
BEGIN
  FOR i := 0 TO 10 DO
    WriteString(dispositivos[i].nombre);
    WriteCard(i);
    WriteLn;
  END;
END EscribaDispositivos;

BEGIN
  dispositivos[0].nombre := "Teclado";
  dispositivos[1].nombre := "Ratón";
  dispositivos[2].nombre := "Monitor";
  dispositivos[3].nombre := "Impresora";
  dispositivos[4].nombre := "Disco Duro";

  EscribaDispositivos;
END DispositivosDeEntradaYSalida.

```

Este código crea un tipo de dato llamado "Dispositivos" que es un array de 5 caracteres. Luego crea un tipo de dato llamado "Dispositivo" que es un registro que contiene un campo llamado "nombre" que es de tipo "Dispositivos". Luego crea un tipo de dato llamado "DispositivosList" que es un array de 11 elementos de tipo "Dispositivo".

La variable "dispositivos" es de tipo "DispositivosList" y se inicializa con los nombres de los 5 dispositivos de entrada y salida más comunes.

El procedimiento "EscribaDispositivos" recorre el array "dispositivos" y escribe el nombre de cada dispositivo en una línea separada.

El programa principal llama al procedimiento "EscribaDispositivos" para escribir los nombres de los dispositivos de entrada y salida en la consola.