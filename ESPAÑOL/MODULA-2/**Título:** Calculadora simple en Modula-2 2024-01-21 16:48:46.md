```modula-2
MODULE Calculadora;
FROM FormatString IMPORT FormatString;

PROCEDURE Calcula(s: STRING): REAL;
VAR n, f: REAL;
    i: CARDINAL;
    op: CHAR;
BEGIN
  FOR i := 0 TO s.Length - 1 DO
    BEGIN
      IF s[i + 1] = '+' OR s[i + 1] = '-' OR s[i + 1] = '*' OR s[i + 1] = '/'  THEN
        op := s[i + 1];
        EXIT;
      END;
    END;
  
  { n = SUMA }
  n := Str2Int(s[0]);
  
  { f = OPERADOR NUMERO }
  f := Str2Int(s[op-1]);
  
  CASE op OF
    '+': BEGIN n := n + f; END
    '-': BEGIN n := n - f; END
    '*': BEGIN n := n * f; END
    '/': BEGIN n := n / f; END
  END;
  
  RETURN n;
END Calcula;

PROCEDURE Main;
VAR total: STRING;
    i: CARDINAL;
    op: CHAR;
    n: REAL;
BEGIN
  { INPUT }
  WriteLn("¿Cuál es la operación que desea realizar?");
  ReadLn(total);
  
  {LOOP}
  REPEAT
    op := total[i + 1];
    n := Calcula(total[0..i]);
    i := i + 3;
    
    CASE op OF
      '+': BEGIN n := n + Str2Int(total[op - 1]); END
      '-': BEGIN n := n - Str2Int(total[op - 1]); END
      '*': BEGIN n := n * Str2Int(total[op - 1]); END
      '/': BEGIN n := n / Str2Int(total[op - 1]); END
    END;
  
  UNTIL i = total.Length-1;
  
  { OUTPUT }
  FormatString(6, " RESULTADO: %.2f", n);
END Main.
```

Explicación:

- `Calcula` es una función que recibe una cadena de texto que contiene una operación matemática y devuelve el resultado de la operación.

- `Calcula` divide la cadena de texto en dos partes, el primer número y el segundo número, y luego utiliza el operador `op` para realizar la operación.

- `Main` es la función principal del programa.

- `Main` recibe del usuario una cadena de texto que contiene una operación matemática y luego llama a la función `Calcula` para obtener el resultado de la operación.

- `Main` muestra el resultado de la operación al usuario.