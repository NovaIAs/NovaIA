```modula-2
MODULE EjemploComplejo;
FROM Terminal IMPORT Write;

TYPE InfoTipo = ARRAY[0..6] OF CARDINAL;

TYPE DatoTipo = RECORD
	nombre: ARRAY[0..100] OF CHAR;
	valores: InfoTipo;
END;

VAR dato: ARRAY[0..10] OF DatoTipo;

PROCEDURE MuestraDato(i: CARDINAL);
BEGIN
	Write("Nombre: ");
	Write(dato[i].nombre);
	NewLine;
	Write("Valores: ");
	FOR j := 0 TO 6 DO
		Write(dato[i].valores[j]);
		IF j < 6 THEN Write(", ");
	END;
	NewLine
END MuestraDato;

PROCEDURE main;
VAR i: ARRAY[0..10] OF CARDINAL;
BEGIN
	InitSched;
	InitTerminal;

	FOR i := 0 TO 10 DO
		dato[i].nombre := "Dato " + IntToStr(i + 1);
		FOR j := 0 TO 6 DO
			dato[i].valores[j] := Random(1000)
		END
	END;

	FOR i := 0 TO 10 DO
		MuestraDato(i)
	END;

	Write("Presione una tecla para salir...");
	ReadChar;
	Done;
END main.
```

Explicación:

* El código define un tipo de datos complejo llamado `DatoTipo` que contiene un campo `nombre` que es un arreglo de caracteres y un campo `valores` que es un arreglo de números enteros.
* El código define un arreglo de variables de tipo `DatoTipo` llamado `dato`.
* El código define un procedimiento llamado `MuestraDato` que se utiliza para mostrar el contenido de una variable de tipo `DatoTipo`.
* El código define un procedimiento llamado `main` que se ejecuta cuando se inicia el programa.
* El procedimiento `main` genera valores aleatorios para los campos `nombre` y `valores` de cada variable en el arreglo `dato`.
* El procedimiento `main` llama al procedimiento `MuestraDato` para mostrar el contenido de cada variable en el arreglo `dato`.
* El procedimiento `main` espera que el usuario presione una tecla para salir del programa.