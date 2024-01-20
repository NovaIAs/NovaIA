```modula-2
MODULE Calculadora;
FROM Terminal IMPORT print, println;

PROCEDURE LeerNumero(VAR numero: CARDINAL): BOOLEAN;
VAR caracter: CARDINAL;
    numeroLeido: CARDINAL := 0;
    señal: BOOLEAN := TRUE;
BEGIN
    REPEAT
        caracter := Terminal.InputCharacter;
        IF caracter = 45 THEN
            señal := FALSE;
        ELSIF caracter >= 48 AND caracter <= 57 THEN
            numeroLeido := numeroLeido * 10 + caracter - 48;
        ELSE
            RETURN FALSE;
        END;
    UNTIL caracter = 13;
    numero := numeroLeido;
    RETURN TRUE;
END LeerNumero;

PROCEDURE MostrarResultado(resultado: CARDINAL);
BEGIN
    Terminal.Print("Resultado: ");
    Terminal.PrintCardinal(resultado);
    Terminal.PrintLn;
END MostrarResultado;

VAR continuar: BOOLEAN;

REPEAT
    continuar := FALSE;

    VAR operador: CHAR;
    VAR numero1: CARDINAL;
    VAR numero2: CARDINAL;
    VAR resultado: CARDINAL;

    Terminal.PrintLn("Calculadora");

    WHILE NOT LeerNumero(numero1) DO
        Terminal.PrintLn("Error: El primer número no es válido.");
    END;

    Terminal.PrintLn("Operador:");
    operador := Terminal.InputLine;

    WHILE NOT LeerNumero(numero2) DO
        Terminal.PrintLn("Error: El segundo número no es válido.");
    END;

    CASE operador OF
        "+": resultado := numero1 + numero2;
        "-": resultado := numero1 - numero2;
        "*": resultado := numero1 * numero2;
        "/": resultado := numero1 / numero2;
        ELSE: resultado := 0;
    END;

    MostrarResultado(resultado);

    Terminal.PrintLn("¿Quieres continuar? (s/n)");
    continuar := Terminal.InputLine = "s";
UNTIL NOT continuar;
END Calculadora.
```

Explicación:

* El módulo Calculadora contiene procedimientos y variables globales.
* El procedimiento LeerNumero() lee un número entero desde la entrada estándar y lo devuelve en la variable numero.
* El procedimiento MostrarResultado() muestra el resultado de la operación en la salida estándar.
* La variable continuar se utiliza para controlar si el usuario quiere seguir usando la calculadora.
* El bucle principal de la calculadora lee dos números, un operador y muestra el resultado de la operación.
* El procedimiento CASE operador evalúa el operador y realiza la operación correspondiente.
* La calculadora se detiene cuando el usuario introduce la letra "n" en respuesta a la pregunta "¿Quieres continuar?".