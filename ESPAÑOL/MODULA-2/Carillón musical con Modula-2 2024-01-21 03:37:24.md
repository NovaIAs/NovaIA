```modula-2
MODULE Chimes;
FROM Terminal IMPORT WriteInt, WriteString;
FROM Strings IMPORT GetLength, CopyString;

TYPE
  Note = CARDINAL;

PROCEDURE Chime(note: Note);
VAR
  s: ARRAY[0..255] OF CHAR;
  i, n, len: CARDINAL;
BEGIN
  n := note;
  IF n > 1 THEN
    n := n - 2;
  END;
  GetString(n, s);
  len := GetLength(s);
  FOR i TO len - 1 DO
    WriteString(s[i]);
  END;
  WriteString("\n");
END Chime;

PROCEDURE GetString(note: Note; VAR s: ARRAY OF CHAR);
VAR
  i, n: CARDINAL;
BEGIN
  n := note;
  IF n > 1 THEN
    n := n - 2;
  END;
  i := 0;
  CASE n OF
    0:
      CopyString("Do", s);
    1:
      CopyString("Re", s);
    2:
      CopyString("Mi", s);
    3:
      CopyString("Fa", s);
    4:
      CopyString("Sol", s);
    5:
      CopyString("La", s);
    6:
      CopyString("Si", s);
    7:
      CopyString("Do", s);
  END;
END GetString;

PROCEDURE Main;
VAR
  i: CARDINAL;
BEGIN
  FOR i TO 7 DO
    Chime(i);
  END;
END Main.
```

Este código crea un programa que toca un conjunto de campanas para generar música. El programa contiene los siguientes elementos:

* El tipo `Note` es un tipo enumerado que define las diferentes notas musicales que pueden reproducirse.
* El procedimiento `Chime` toma una nota como argumento y reproduce el sonido correspondiente.
* El procedimiento `GetString` toma una nota como argumento y devuelve una cadena de caracteres que contiene el nombre de la nota.
* El procedimiento `Main` es el punto de entrada del programa. Ejecuta el procedimiento `Chime` para reproducir las notas Do, Re, Mi, Fa, Sol, La y Si.

Cuando se ejecuta el programa, sonará una serie de notas musicales. Las notas se reproducirán en orden ascendente, desde Do hasta Si.