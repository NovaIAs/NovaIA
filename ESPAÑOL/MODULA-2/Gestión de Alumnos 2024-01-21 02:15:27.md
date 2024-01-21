```modula-2

MODULE GestionAlumnos;
FROM Terminal IMPORT WriteString, WriteLn, ReadString, WriteInt, ReadInt,
                   WriteReal, ReadReal, WriteChar, ReadChar, NewLine;

TYPE Alumno = RECORD
                 nombre: ARRAY 30 OF CHAR;
                 nota: REAL;
              END;

VAR alumnos: ARRAY 100 OF Alumno;
    numalumnos: CARDINAL;

PROCEDURE LeerAlumno(VAR alumno: Alumno);
BEGIN
    WriteString("Nombre: ");
    ReadString(alumno.nombre);
    WriteString("Nota: ");
    ReadReal(alumno.nota);
END LeerAlumno;

PROCEDURE MostrarAlumno(alumno: Alumno);
BEGIN
    WriteString("Nombre: ");
    WriteLn(alumno.nombre);
    WriteString("Nota: ");
    WriteLn(alumno.nota);
END MostrarAlumno;

PROCEDURE LeerAlumnos;
VAR i: CARDINAL;
BEGIN
    WriteString("¿Cuántos alumnos quieres introducir? ");
    numalumnos := ReadInt;
    FOR i := 1 TO numalumnos DO
        LeerAlumno(alumnos[i]);
    END;
END LeerAlumnos;

PROCEDURE MostrarAlumnos;
VAR i: CARDINAL;
BEGIN
    FOR i := 1 TO numalumnos DO
        MostrarAlumno(alumnos[i]);
    END;
END MostrarAlumnos;

PROCEDURE CalcularMedia;
VAR media: REAL;
    i: CARDINAL;
BEGIN
    media := 0.0;
    FOR i := 1 TO numalumnos DO
        media := media + alumnos[i].nota;
    END;
    media := media / numalumnos;
    WriteString("La media de las notas es: ");
    WriteLn(media);
END CalcularMedia;

PROCEDURE OrdenarAlumnos;
VAR i, j: CARDINAL;
    temp: Alumno;
BEGIN
    FOR i := 1 TO numalumnos - 1 DO
        FOR j := i + 1 TO numalumnos DO
            IF alumnos[i].nota < alumnos[j].nota THEN
                temp := alumnos[i];
                alumnos[i] := alumnos[j];
                alumnos[j] := temp;
            END;
        END;
    END;
END OrdenarAlumnos;

PROCEDURE BuscarAlumno;
VAR nombre: ARRAY 30 OF CHAR;
    i: CARDINAL;
    encontrado: BOOLEAN;
BEGIN
    WriteString("Introduce el nombre del alumno que quieres buscar: ");
    ReadString(nombre);
    encontrado := FALSE;
    FOR i := 1 TO numalumnos DO
        IF alumnos[i].nombre = nombre THEN
            encontrado := TRUE;
            MostrarAlumno(alumnos[i]);
        END;
    END;
    IF NOT encontrado THEN
        WriteString("Alumno no encontrado");
    END;
END BuscarAlumno;

BEGIN
    LeerAlumnos;
    MostrarAlumnos;
    CalcularMedia;
    OrdenarAlumnos;
    MostrarAlumnos;
    BuscarAlumno;
END GestionAlumnos.

```

Este código es un programa en MODULA-2 que gestiona una lista de alumnos. El programa permite leer los datos de los alumnos, mostrarlos, calcular la media de las notas, ordenarlos por nota y buscar un alumno por su nombre.

El programa define un tipo de dato llamado Alumno que contiene el nombre y la nota de un alumno. También define un array de alumnos llamado alumnos que puede almacenar hasta 100 alumnos.

La variable numalumnos contiene el número de alumnos que hay en la lista.

El procedimiento LeerAlumno lee los datos de un alumno y los almacena en la variable alumno.

El procedimiento MostrarAlumno muestra los datos de un alumno.

El procedimiento LeerAlumnos lee los datos de los alumnos y los almacena en el array alumnos.

El procedimiento MostrarAlumnos muestra los datos de los alumnos que hay en el array alumnos.

El procedimiento CalcularMedia calcula la media de las notas de los alumnos y la muestra por pantalla.

El procedimiento OrdenarAlumnos ordena los alumnos por nota de mayor a menor.

El procedimiento BuscarAlumno busca un alumno por su nombre y muestra sus datos si lo encuentra.

El programa principal llama a los procedimientos LeerAlumnos, MostrarAlumnos, CalcularMedia, OrdenarAlumnos, MostrarAlumnos y BuscarAlumno para gestionar la lista de alumnos.