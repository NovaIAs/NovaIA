```modula-2
MODULE ManejadorDeArchivos;
FROM Formatos import Estructura;
FROM Utilidades import Utilidades;

TYPE Fecha = CARDINAL;
TYPE Hora = CARDINAL;
TYPE Archivo = ARRAY OF Estructura;

VAR Archivos: ARRAY OF Archivo;
VAR NumeroDeArchivos: CARDINAL;
VAR ArchivoActual: CARDINAL;

PROCEDURE AbrirArchivo(Nombre: ARRAY OF CHAR);
VAR Archivo: Archivo;
VAR Error: BOOLEAN;
BEGIN
    Error := TRUE;
    FOR i := 0 TO NumeroDeArchivos - 1 DO
        IF Archivos[i].Nombre = Nombre THEN
            Error := FALSE;
            ArchivoActual := i;
        END
    END;
    IF Error THEN
        NumeroDeArchivos := NumeroDeArchivos + 1;
        ArchivoActual := NumeroDeArchivos - 1;
        Archivos[ArchivoActual].Nombre := Nombre;
        Archivos[ArchivoActual].Registros := 0;
    END;
END AbrirArchivo;

PROCEDURE CerrarArchivo;
BEGIN
    Archivos[ArchivoActual].Nombre := "";
    Archivos[ArchivoActual].Registros := 0;
END CerrarArchivo;

PROCEDURE AgregarRegistro(Dato: Estructura);
VAR Registro: Estructura;
VAR Tamaño: CARDINAL;
VAR Archivo: FILE;

BEGIN
    Archivos[ArchivoActual].Registros := Archivos[ArchivoActual].Registros + 1;
    Tamaño := SIZEOF(Estructura);
    OPEN(Archivo, Archivos[ArchivoActual].Nombre, WRITE);
    Registro.Numero := Archivos[ArchivoActual].Registros;
    Registro.Datos := Dato.Datos;
    Registro.Fecha := Dato.Fecha;
    Registro.Hora := Dato.Hora;
    WRITE(Archivo, Registro, Tamaño);
    CLOSE(Archivo);
END AgregarRegistro;

PROCEDURE LeerRegistro(Numero: CARDINAL; VAR Dato: Estructura);
VAR Error: BOOLEAN;
VAR Tamaño: CARDINAL;
VAR Archivo: FILE;

BEGIN
    Error := TRUE;
    FOR i := 0 TO Archivos[ArchivoActual].Registros - 1 DO
        IF Archivos[ArchivoActual].Registros[i].Numero = Numero THEN
            Error := FALSE;
            Dato := Archivos[ArchivoActual].Registros[i];
        END
    END;
    IF Error THEN
        Dato.Numero := 0;
        Dato.Datos := "";
        Dato.Fecha := 0;
        Dato.Hora := 0;
    ELSE
        OPEN(Archivo, Archivos[ArchivoActual].Nombre, READ);
        SEEK(Archivo, (Numero - 1) * Tamaño);
        READ(Archivo, Dato, Tamaño);
        CLOSE(Archivo);
    END;
END LeerRegistro;

PROCEDURE EliminarRegistro(Numero: CARDINAL);
VAR i: CARDINAL;
VAR Tamaño: CARDINAL;
VAR Archivo: FILE;

BEGIN
    i := 0;
    WHILE i < Archivos[ArchivoActual].Registros - 1 AND
        Archivos[ArchivoActual].Registros[i].Numero <> Numero DO
        i := i + 1
    END;
    IF i < Archivos[ArchivoActual].Registros - 1 THEN
        Tamaño := SIZEOF(Estructura);
        OPEN(Archivo, Archivos[ArchivoActual].Nombre, WRITE);
        SEEK(Archivo, i * Tamaño);
        DELETE(Archivo, Tamaño);
        CLOSE(Archivo);
        Archivos[ArchivoActual].Registros := Archivos[ArchivoActual].Registros - 1;
    END;
END EliminarRegistro;

PROCEDURE ListarArchivos;
VAR i: CARDINAL;

BEGIN
    FOR i := 0 TO NumeroDeArchivos - 1 DO
        writeln(Archivos[i].Nombre);
    END;
END ListarArchivos;

PROCEDURE ListarRegistros;
VAR i: CARDINAL;
VAR Dato: Estructura;

BEGIN
    FOR i := 0 TO Archivos[ArchivoActual].Registros - 1 DO
        LeerRegistro(i + 1, Dato);
        writeln(Dato.Numero, Dato.Datos, Dato.Fecha, Dato.Hora);
    END;
END ListarRegistros;

BEGIN
    NumeroDeArchivos := 0;
    ArchivoActual := -1;
    WHILE TRUE DO
        writeln("1. Abrir archivo");
        writeln("2. Cerrar archivo");
        writeln("3. Agregar registro");
        writeln("4. Leer registro");