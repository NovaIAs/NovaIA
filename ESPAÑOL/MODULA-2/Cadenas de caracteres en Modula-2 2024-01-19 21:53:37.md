```modula-2
MODULE Strings;

FROM InOut IMPORT WriteString, WriteLn;

TYPE String = ARRAY OF CHAR;

PROCEDURE Length(s: String): CARDINAL;
BEGIN
  RETURN CARDINAL(s[0])
END Length;

PROCEDURE Concat(s1: String; s2: String): String;
VAR
  i: CARDINAL;
  l1, l2: CARDINAL;
BEGIN
  l1 := Length(s1); l2 := Length(s2);
  RETURN ARRAY OF CHAR FROM 0 TO l1 + l2 - 1 OF
    WITH k FROM 0 TO LENGTH(s1) - 1 DO
      s1[k]
    END,
    WITH k FROM LENGTH(s1) TO l1 + l2 - 1 DO
      s2[k - LENGTH(s1)]
    END
END Concat;

PROCEDURE Index(s: String; c: CHAR): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO LENGTH(s) - 1 DO
    IF s[i] = c THEN RETURN i END
  END;
  RETURN -1
END Index;

PROCEDURE Substring(s: String; i, l: CARDINAL): String;
VAR
  j: CARDINAL;
BEGIN
  RETURN ARRAY OF CHAR FROM 0 TO l - 1 OF
    WITH k FROM 0 TO l - 1 DO
      s[i + k]
    END
  END
END Substring;

BEGIN
  WriteString("Hola ");
  WriteString(Concat("mundo, ", "cómo estás?"));
  WriteLn;
  WriteString("El índice de la letra 'o' en 'hola' es ");
  WriteCard(Index("hola", 'o'));
  WriteLn;
  WriteString("El substring de 'mundo' desde el índice 2 hasta el índice 4 es ");
  WriteString(Substring("mundo", 2, 4));
  WriteLn
END Strings.
```

El código anterior es un módulo de Modula-2 que define un tipo de datos String, así como varias operaciones sobre cadenas de caracteres. El módulo también incluye un programa principal que demuestra cómo utilizar las operaciones definidas.

El tipo de datos String es una matriz de caracteres. La operación Length devuelve la longitud de una cadena de caracteres. La operación Concat concatena dos cadenas de caracteres. La operación Index devuelve el índice de la primera aparición de un carácter en una cadena de caracteres. La operación Substring devuelve una subcadena de una cadena de caracteres dada.

El programa principal crea tres cadenas de caracteres: "Hola", "mundo" y "cómo estás?". Luego, el programa concatena las cadenas "Hola" y "mundo" y las imprime en la consola. A continuación, el programa busca el índice de la letra 'o' en la cadena "hola" e imprime el índice en la consola. Por último, el programa extrae la subcadena "un" de la cadena "mundo" e imprime la subcadena en la consola.