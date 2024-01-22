```modula-2
MODULE SpanishComplexCode;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString, WriteCard, WriteLn;

VAR
  StringArray: ARRAY 10 OF STRING;
  i, j: CARDINAL;

PROCEDURE FillStringArray;
BEGIN
  FOR i := 1 TO 10 DO
    StringArray[i] := FormatString("%02d", i);
  END;
END FillStringArray;

PROCEDURE PrintStringArray;
BEGIN
  FOR i := 1 TO 10 DO
    WriteString(StringArray[i]);
    IF i MOD 5 = 0 THEN
      WriteLn;
    ELSE
      WriteCard(2, ' ');
    END;
  END;
END PrintStringArray;

PROCEDURE Main;
VAR
  StringToReverse: STRING;
  ReversedString: STRING;
BEGIN
  WriteString("Introduce una cadena: ");
  ReadLn(StringToReverse);

  ReversedString := "";
  FOR i := LENGTH(StringToReverse) TO -1 BY -1 DO
    ReversedString := ReversedString + StringToReverse[i];
  END;

  WriteString("Cadena original: ");
  WriteString(StringToReverse);
  WriteLn;
  WriteString("Cadena invertida: ");
  WriteString(ReversedString);
  WriteLn;
END Main;

BEGIN
  FillStringArray;
  PrintStringArray;
  Main;
END SpanishComplexCode.
```

El código anterior es un programa complejo en MODULA-2 que realiza varias tareas:

1. Llena una matriz de cadenas con los números del 1 al 10.
2. Imprime la matriz de cadenas en la consola.
3. Lee una cadena del usuario y la invierte.
4. Imprime la cadena original y la cadena invertida en la consola.

El código está bien documentado y utiliza varias características del lenguaje MODULA-2, como los tipos de datos definidos por el usuario, los procedimientos y los bucles.

El código también utiliza el módulo `FormatString` para convertir un número a una cadena y el módulo `Terminal` para escribir en la consola.