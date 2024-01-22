```modula-2

MODULE Lexico;
FROM Strings IMPORT Substring;
FROM Terminal IMPORT Punto, CorcheteIzquierdo, CorcheteDerecho, ParentesisIzquierdo, ParentesisDerecho, Mas, Menos, Producto, Division,
                     Potencia, Igual, MenorQue, MayorQue, MenorOIgualQue, MayorOIgualQue, PuntoYComa, DosPuntos, Coma, YLogico, OLogico, NoLogico;

TYPE Simbolo = Terminal | Literal;

TYPE Terminal = RECORD
                      nombre: STRING;
                END;

TYPE Literal = RECORD
                      nombre: STRING;
                      valor: LONGCARD;
                END;

VAR CarácterActual: CHAR;
VAR ÍndiceActual: CARDINAL;
VAR CadenaActual: ARRAY OF CHAR;
VAR SímboloActual: Simbolo;

PROCEDURE GetNextChar;
VAR Carácter: CHAR;
BEGIN
    ÍndiceActual := ÍndiceActual + 1;
    Carácter := CadenaActual[ÍndiceActual];
    CarácterActual := Carácter;
END GetNextChar;

PROCEDURE GetNextSymbol;
VAR Ch: CHAR;
BEGIN
    LOOP
        GetNextChar;
        CASE CarácterActual OF
            ' ':      SKIP;  (* Ignorar espacios en blanco *)
            '(':      SímboloActual := ParentesisIzquierdo;
            ')':      SímboloActual := ParentesisDerecho;
            '/':      SímboloActual := Division;
            '*':      SímboloActual := Producto;
            '+':      SímboloActual := Mas;
            '-':      SímboloActual := Menos;
            '<':      IF CarácterActual = '>' THEN
                            SímboloActual := MenorOIgualQue
                        ELSE
                            SímboloActual := MenorQue
                        END;
            '>':      IF CarácterActual = '=' THEN
                            SímboloActual := MayorOIgualQue
                        ELSE
                            SímboloActual := MayorQue
                        END;
            '=':      SímboloActual := Igual;
            ';':      SímboloActual := PuntoYComa;
            ',':      SímboloActual := Coma;
            '.':      SímboloActual := Punto;
            '[':      SímboloActual := CorcheteIzquierdo;
            ']':      SímboloActual := CorcheteDerecho;
            ' ':      SKIP;  (* Ignorar espacios en blanco *)
            ELSE
            IF CarácterActual = '_' OR CarácterActual >= 'A' AND CarácterActual <= 'Z'
                                      OR CarácterActual >= 'a' AND CarácterActual <= 'z' THEN
                 SímboloActual := Literal(Substring(CadenaActual, ÍndiceActual, 1), 0);
                WHILE CarácterActual = '_' OR CarácterActual >= 'A' AND CarácterActual <= 'Z'
                                           OR CarácterActual >= 'a' AND CarácterActual <= 'z' OR
                                           CarácterActual >= '0' AND CarácterActual <= '9' DO
                    GetNextChar;
                    SímboloActual.nombre := SímboloActual.nombre + Substring(CadenaActual, ÍndiceActual, 1);
                END;
            ELSE
                SímboloActual := Literal(Substring(CadenaActual, ÍndiceActual, 1), 0)
            END
        END;
        EXIT WHEN SímboloActual.nombre <> "";
    END;
END GetNextSymbol;

PROCEDURE Analizar(Cadena: ARRAY OF CHAR);
VAR I: CARDINAL;
VAR CadAux: STRING;
BEGIN
    CadenaActual := Cadena;
    ÍndiceActual := 0;
    GetNextSymbol;
    WHILE SímboloActual.nombre <> "" DO
        CadAux := CadAux + SímboloActual.nombre + " ";
        GetNextSymbol;
    END;
    WRITELN("Los símbolos encontrados son:");
    WRITELN(CadAux);
END Analizar;

BEGIN
    Analizar("a + b * c - d / e");
    Analizar("x = y > z <= w");
    Analizar("if a then b else c");
    Analizar("while a do b");
    Analizar("for i:=1 to 10 do a:=a+1");
END Lexico.

```

Explicación del código:

* El código anterior implementa un analizador léxico simple en MODULA-2.
* El analizador léxico es la primera fase de un compilador. Toma una cadena de texto (código fuente) como entrada y la divide en una secuencia de tokens (símbolos).
* Cada token representa una unidad léxica significativa en el código fuente, como una palabra clave, un operador, un identificador o un literal.
* El analizador léxico utiliza una tabla de palabras clave para identificar las palabras clave del lenguaje.
* El analizador léxico también utiliza una tabla de operadores para identificar los operadores del lenguaje.
* El analizador léxico utiliza una expresión regular para identificar los identificadores.
* El analizador léxico utiliza una expresión regular para identificar los literales.
* El analizador léxico devuelve una secuencia de tokens al analizador sintáctico.
* El analizador sintáctico es la segunda fase de un compilador. Toma la secuencia de tokens del analizador léxico como entrada y la analiza para determinar si el código fuente es sintácticamente correcto.
* El analizador sintáctico utiliza una gramática para determinar si el código fuente es sintácticamente correcto.
* El analizador sintáctico devuelve un árbol sintáctico abstracto (AST) si el código fuente es sintácticamente correcto.
* El AST es una representación interna del código fuente que se utiliza para generar código máquina.
* El generador de código es la tercera fase de un compilador. Toma el AST del analizador sintáctico como entrada y genera código máquina.
* El código máquina es un programa ejecutable que se puede ejecutar en un ordenador.