```modula-2
MODULE GestiónDeAlumnos;
FROM Terminal IMPORT WriteString, WriteLn, ReadString, ReadInt;
FROM FormatStrings IMPORT Format;

TYPE Curso = ARRAY 1..10 OF RECORD
    Nombre: ARRAY 1..20 OF CHAR;
    Nota: ARRAY 1..10 OF INTEGER;
    Promedio: REAL;
END;

VAR Alumnos: ARRAY 1..10 OF Curso;
VAR NumAlumnos: INTEGER;

PROCEDURE ObtenerDatosAlumnos;
VAR i, j, nota: INTEGER;
BEGIN
    WriteString("¿Cuántos alumnos desea gestionar? ");
    NumAlumnos := ReadInt;
    FOR i := 1 TO NumAlumnos DO
        WriteString("Introduzca el nombre del alumno: ");
        ReadString(Alumnos[i].Nombre);
        FOR j := 1 TO 10 DO
            WriteString(Format("Introduzca la nota %d: ", [j]));
            nota := ReadInt;
            Alumnos[i].Nota[j] := nota;
        END;
    END;
END ObtenerDatosAlumnos;

PROCEDURE CalcularPromedios;
VAR i, j, suma: INTEGER;
BEGIN
    FOR i := 1 TO NumAlumnos DO
        suma := 0;
        FOR j := 1 TO 10 DO
            suma := suma + Alumnos[i].Nota[j];
        END;
        Alumnos[i].Promedio := suma / 10.0;
    END;
END CalcularPromedios;

PROCEDURE MostrarDatosAlumnos;
VAR i: INTEGER;
BEGIN
    WriteStringLn("Datos de los alumnos:");
    FOR i := 1 TO NumAlumnos DO
        WriteStringLn(Format("Nombre: %s", [Alumnos[i].Nombre]));
        WriteStringLn("Notas:");
        FOR j := 1 TO 10 DO
            WriteStringLn(Format("Nota %d: %d", [j, Alumnos[i].Nota[j]]));
        END;
        WriteStringLn(Format("Promedio: %.2f", [Alumnos[i].Promedio]));
        WriteStringLn("");
    END;
END MostrarDatosAlumnos;

BEGIN
    ObtenerDatosAlumnos;
    CalcularPromedios;
    MostrarDatosAlumnos;
END GestiónDeAlumnos.
```

Explicación:

* El módulo `GestiónDeAlumnos` define un tipo de datos personalizado llamado `Curso`, que representa la información de un curso. Cada curso tiene un nombre y un array de 10 notas.
* La variable `Alumnos` es un array de 10 cursos.
* La variable `NumAlumnos` almacena el número de alumnos que se van a gestionar.
* El procedimiento `ObtenerDatosAlumnos` solicita al usuario que introduzca los datos de los alumnos.
* El procedimiento `CalcularPromedios` calcula el promedio de las notas de cada alumno.
* El procedimiento `MostrarDatosAlumnos` muestra los datos de los alumnos, incluyendo el nombre, las notas y el promedio.
* El programa principal llama a los procedimientos `ObtenerDatosAlumnos`, `CalcularPromedios` y `MostrarDatosAlumnos` para gestionar los datos de los alumnos.