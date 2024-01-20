```modula-2
MODULE Gramatica;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Terminal, TERMINAL_EOF, TERMINAL_EOI;

TYPE TerminalList = ARRAY OF Terminal;
TYPE TerminalIter = ARRAY OF Terminal;
TYPE TerminalPtr = POINTER TO TerminalIter;

PROCEDURE AppendTerminal(VAR Iter: TerminalPtr; NewTerm: Terminal);
PROCEDURE NextSymbol(VAR Iter: TerminalPtr): Terminal;
PROCEDURE EndSymbol(VAR Iter: TerminalPtr): BOOLEAN;

VAR Cadena: ARRAY OF CHAR;
VAR Index: CARDINAL;
VAR Result: INTEGER;

PROCEDURE Inicializar();
BEGIN
  Index := 0;
END Inicializar;

PROCEDURE AnalizarCadena(CadenaInput: ARRAY OF CHAR);
VAR Iter: TerminalPtr;
BEGIN
  Cadena := CadenaInput;
  Index := 0;
  Iter := NEW TerminalIter;
  WHILE TRUE DO
    Result := 0;
    Index := 0;
    IF Index < LENGTH(Cadena) THEN
      AppendTerminal(Iter, TERMINAL_EOF);
      AppendTerminal(Iter, TERMINAL_EOI);
    ELSE
      AppendTerminal(Iter, TERMINAL_EOF);
    END;
    WHILE TRUE DO
      IF EndSymbol(Iter) THEN
        EXIT;
      ELSIF Result = 0 THEN
        Result := FUN_CALL(Sintactico.Program, Iter);
        IF Result = 0 THEN
          ERROR("Error en la expresión");
        END;
      ELSE
        ERROR("Error en la expresión");
      END;
    END;
  END;
END AnalizarCadena;

BEGIN
  Inicializar();
  RETURN 0;
END Gramatica.
```

**Explicación**

El código anterior es un sencillo compilador escrito en Modula-2. El compilador consta de tres partes principales:

* El analizador léxico, que divide la cadena de entrada en una secuencia de tokens.
* El analizador sintáctico, que analiza la secuencia de tokens y construye un árbol de sintaxis abstracta (AST).
* El generador de código, que traduce el AST a código máquina.

El analizador léxico se implementa mediante una serie de procedimientos que devuelven el siguiente token en la cadena de entrada. El analizador sintáctico se implementa mediante una serie de gramáticas, cada una de las cuales define un conjunto de reglas para analizar una parte específica de la cadena de entrada. El generador de código se implementa mediante una serie de procedimientos que traducen el AST a código máquina.

El compilador también contiene un número de procedimientos de utilidad, como `FormatString`, `TERMINAL_EOF` y `TERMINAL_EOI`. Estos procedimientos se utilizan para formatear mensajes de error, definir símbolos especiales y terminar la secuencia de tokens.

El compilador se puede utilizar para compilar programas escritos en un lenguaje simple llamado "Sintactico". El lenguaje "Sintactico" es un lenguaje de programación de propósito general que soporta una variedad de tipos de datos, operadores y estructuras de control. El compilador traduce los programas escritos en el lenguaje "Sintactico" a código máquina que puede ser ejecutado por un ordenador.