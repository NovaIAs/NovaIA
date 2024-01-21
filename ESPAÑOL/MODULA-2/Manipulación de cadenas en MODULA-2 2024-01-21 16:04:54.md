```modula-2
MODULE StringHandler;

FROM FormatString IMPORT FormatString;

PROCEDURE StringToInt(s: ARRAY OF CHAR): INTEGER;
VAR
  i, n: INTEGER;
  neg: BOOLEAN;
BEGIN
  i := 0;
  neg := FALSE;
  IF s[i] = '-' THEN
    neg := TRUE;
    i := i + 1;
  END;
  n := 0;
  WHILE i < s[0] DO
    n := n * 10 + CHARTOINT(s[i]);
    i := i + 1;
  END;
  IF neg THEN
    n := -n;
  END;
  RETURN n;
END StringToInt;

PROCEDURE IntToString(n: INTEGER; s: ARRAY OF CHAR);
VAR
  i: INTEGER;
  neg: BOOLEAN;
BEGIN
  i := 0;
  neg := FALSE;
  IF n < 0 THEN
    neg := TRUE;
    n := -n;
  END;
  WHILE n > 0 DO
    i := i + 1;
    s[i] := INTTOCHAR(n MOD 10);
    n := n DIV 10;
  END;
  IF neg THEN
    i := i + 1;
    s[i] := '-';
  END;
  s[i + 1] := CHR(0);
  REVERSE(s,1,i);
END IntToString;

PROCEDURE ArrayToString(s: ARRAY OF CHAR; n: INTEGER): STRING;
VAR
  i: INTEGER;
BEGIN
  RETURN COPY(s,1,n);
END ArrayToString;

PROCEDURE StringToArray(s: STRING; n: INTEGER; a: ARRAY OF CHAR);
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO n DO
    a[i] := s[i];
  END;
END StringToArray;

PROCEDURE FormatInt(n: INTEGER; f: ARRAY OF CHAR): STRING;
VAR
  buf: ARRAY[0..100] OF CHAR;
BEGIN
  IntToString(n,buf);
  RETURN FormatString(f,buf);
END FormatInt;

PROCEDURE FormatReal(r: REAL; f: ARRAY OF CHAR): STRING;
VAR
  buf: ARRAY[0..100] OF CHAR;
BEGIN
  INTOSTRING(r,buf);
  RETURN FormatString(f,buf);
END FormatReal;

END StringHandler.
```

Este código es una colección de procedimientos y funciones útiles para manipular cadenas de caracteres en MODULA-2.

El primer procedimiento, `StringToInt`, convierte una cadena de caracteres en un número entero. Lo hace iterando sobre la cadena, convirtiendo cada carácter en un dígito y multiplicando el resultado por 10 para obtener el valor del número entero. Si el primer carácter de la cadena es un signo menos, el procedimiento lo trata como un valor negativo.

El segundo procedimiento, `IntToString`, convierte un número entero en una cadena de caracteres. Lo hace dividiendo el número entero por 10 repetidamente y concatenando el resto de cada división a una cadena de caracteres. Si el número entero es negativo, el procedimiento antepone un signo menos a la cadena.

El tercer procedimiento, `ArrayToString`, convierte una matriz de caracteres en una cadena de caracteres. Lo hace concatenando todos los caracteres de la matriz en una sola cadena.

El cuarto procedimiento, `StringToArray`, convierte una cadena de caracteres en una matriz de caracteres. Lo hace copiando cada carácter de la cadena en una matriz de caracteres.

El quinto procedimiento, `FormatInt`, formatea un número entero en una cadena de caracteres según una cadena de formato. La cadena de formato puede contener códigos de formato que especifican cómo se debe formatear el número entero.

El sexto procedimiento, `FormatReal`, formatea un número real en una cadena de caracteres según una cadena de formato. La cadena de formato puede contener códigos de formato que especifican cómo se debe formatear el número real.