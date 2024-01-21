```modula-2
MODULE DecodificadorMorse;
FROM InOut IMPORT WriteString, WriteLn, GetChar, WriteInt;

TYPE
  CODIGOSIGNO = ARRAY 5 OF CHAR;
  SEÑAL      = (PUNTO, RAYA, ESPACIOSIGNO, ESPACIOENTREPALABRAS);
  MORSE      = RECORD
                 signo   : CODIGOSIGNO;
                 longitud: INTEGER;
               END;
  ÉXITO      = INTEGER;
  SEÑALIZADA = ARRAY ÉXITO OF SEÑAL;

VAR
  senalizada : SEÑALIZADA;
  simbolo    : CODIGOSIGNO;

PROCEDURE Decodificar;

  VAR
    i       : INTEGER;
    pos     : INTEGER;
    longcad : INTEGER;
    longpal : INTEGER;
    termina : BOOLEAN;
    cadena  : ARRAY 256 OF CHAR;
  BEGIN
    i := 0;
    pos := 0;
    longcad := 0;
    longpal := 0;
    termina := FALSE;

    WHILE NOT termina DO
      ReadChar(cadena[pos]);

      IF cadena[pos] = '_' THEN
        termina := TRUE
      ELSE
        IF cadena[pos] = '.' THEN
          senalizada[i] := PUNTO
        ELSIF cadena[pos] = '-' THEN
          senalizada[i] := RAYA
        ELSIF cadena[pos] = ' ' THEN
          IF longpal = 0 THEN
            senalizada[i] := ESPACIOSIGNO
          ELSE
            senalizada[i] := ESPACIOENTREPALABRAS
          END;
        END;
        i := i + 1;
        pos := pos + 1;
      END;
    END;

    longcad := i;

    pos := 0;
    i := 0;

    WHILE pos < longcad DO
      longpal := 0;

      WHILE (senalizada[pos] # ESPACIOENTREPALABRAS)
           AND (pos < longcad) DO
        simbolo[longpal] := senalizada[pos];
        pos := pos + 1;
        longpal := longpal + 1;
      END;

      simbolo[longpal] := '\0';

      IF longpal # 0 THEN
        WriteString(simbolo);
      END;

      IF senalizada[pos] = ESPACIOENTREPALABRAS THEN
        WriteLn;
        pos := pos + 1;
      END;

      i := i + 1;
    END;

    WriteLn;
  END Decodificar;

BEGIN
  Decodificar;
END DecodificadorMorse.
```

Este código es un decodificador Morse que convierte una cadena de caracteres que representa una señal Morse en su correspondiente texto.

El código define un tipo de datos llamado `SEÑAL` que puede tomar los siguientes valores: `PUNTO`, `RAYA`, `ESPACIOSIGNO` y `ESPACIOENTREPALABRAS`. También define un tipo de datos llamado `MORSE` que es un registro que contiene un `signo` (un array de 5 caracteres) y una `longitud` (un entero).

La variable `senalizada` es un array de `SEÑAL` que contiene la señal Morse codificada. La variable `simbolo` es un array de caracteres que contiene el texto decodificado.

El procedimiento `Decodificar` lee una cadena de caracteres de la entrada estándar y la convierte en una señal Morse codificada. Luego, itera sobre la señal Morse codificada y la decodifica en texto.

El código utiliza un bucle `while` para leer la cadena de caracteres de la entrada estándar. Si el carácter actual es `_`, entonces el bucle termina. Si el carácter actual es `.`, `-` o ` `, entonces el carácter se añade a la señal Morse codificada.

El código también utiliza un bucle `while` para iterar sobre la señal Morse codificada y decodificarla en texto. El bucle `while` termina cuando se llega al final de la señal Morse codificada o cuando se encuentra un espacio en blanco.

Si la longitud del símbolo actual es diferente de 0, entonces el símbolo se escribe en la salida estándar. Si el símbolo actual es un espacio en blanco, entonces se escribe una nueva línea en la salida estándar.

El código finalmente escribe una nueva línea en la salida estándar.