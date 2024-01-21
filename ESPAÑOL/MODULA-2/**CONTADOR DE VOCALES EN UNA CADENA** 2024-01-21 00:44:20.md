```modula-2
MODULE Complejo;
FROM InOut IMPORT WriteString, ReadString;

VAR
  cadena: ARRAY 16 OF CHAR;
  caracter: CHAR;
  longitud: INTEGER;
  indice: INTEGER;
  vocal: BOOLEAN;

PROCEDURE EsVocal(c: CHAR): BOOLEAN;
BEGIN
  RETURN (c = 'A') OR (c = 'E') OR (c = 'I') OR
         (c = 'O') OR (c = 'U') OR (c = 'a') OR
         (c = 'e') OR (c = 'i') OR (c = 'o') OR
         (c = 'u');
END EsVocal;

PROCEDURE ContarVocales(cadena: ARRAY OF CHAR);
VAR
  indice: INTEGER;
  vocal: BOOLEAN;
  contador: INTEGER;
BEGIN
  contador := 0;
  FOR indice := LOW(cadena) TO HIGH(cadena) DO
    caracter := cadena[indice];
    vocal := EsVocal(caracter);
    IF vocal THEN
      contador := contador + 1;
    END;
  END;
  WriteString("La cadena contiene ");
  WriteLongInt(contador, 0);
  WriteString(" vocales.\n");
END ContarVocales;

BEGIN
  WriteString("Ingrese una cadena de máximo 16 caracteres: ");
  ReadString(cadena, 16);
  longitud := LENGTH(cadena);
  IF longitud > 0 THEN
    ContarVocales(cadena);
  END;
END Complejo.
```

Explicación del código:

* El procedimiento `EsVocal` comprueba si un carácter dado es una vocal. Devuelve `TRUE` si el carácter es una vocal y `FALSE` en caso contrario.
* El procedimiento `ContarVocales` cuenta el número de vocales en una cadena de caracteres dada. Recorre la cadena carácter por carácter y llama al procedimiento `EsVocal` para comprobar si cada carácter es una vocal. Si es vocal, incrementa el contador de vocales.
* El programa principal primero pide al usuario que introduzca una cadena de caracteres de máximo 16 caracteres. A continuación, llama al procedimiento `ContarVocales` para contar el número de vocales en la cadena introducida por el usuario.
* El procedimiento `ContarVocales` imprime en la consola el número de vocales encontradas en la cadena introducida por el usuario.