```modula-2
MODULE CalculadoraCompleja;
FROM FormatString IMPORT Format;
FROM RealInOut IMPORT GetR, PutR, PutNL;

PROCEDURE CalculaComplejo(c1, c2: Complex; signo: CHAR; OUT resultado: Complex);
BEGIN
    IF signo = '+' THEN
        resultado.a := c1.a + c2.a;
        resultado.b := c1.b + c2.b;
    ELSIF signo = '-' THEN
        resultado.a := c1.a - c2.a;
        resultado.b := c1.b - c2.b;
    ELSIF signo = '*' THEN
        resultado.a := c1.a*c2.a - c1.b*c2.b;
        resultado.b := c1.b*c2.a + c1.a*c2.b;
    ELSE
        resultado.a := (c1.a*c2.a + c1.b*c2.b) / (c2.a*c2.a + c2.b*c2.b);
        resultado.b := (c1.b*c2.a - c1.a*c2.b) / (c2.a*c2.a + c2.b*c2.b);
    END;
END CalculaComplejo;

TYPE Complex = RECORD a, b: REAL END;

VAR c1, c2, resultado: Complex;
cadena: ARRAY[1..20] OF CHAR;

BEGIN
    WHILE TRUE DO
        WRITE("Primer complejo (a+bi): ");
        GetR(c1.a);
        GetR(c1.b);

        WRITE("Segundo complejo (c+di): ");
        GetR(c2.a);
        GetR(c2.b);

        WRITE("Operador (+, -, *, /): ");
        READ(cadena);
        signo := cadena[1];

        CalculaComplejo(c1, c2, signo, resultado);

        Format("%.2f+%.2fi", resultado.a, resultado.b, cadena);
        WRITE("Resultado: ");
        PutNL;
        PutR(cadena);

        WRITE("Continuar (S/N): ");
        READ(cadena);
        IF cadena[1] = 'N' THEN
            LEAVE;
        END;
    END;
END CalculadoraCompleja.
```

Este código implementa una calculadora compleja en Modula-2. La calculadora permite sumar, restar, multiplicar y dividir números complejos.

El código está dividido en dos módulos:

* **CalculadoraCompleja**: Este módulo contiene el código principal de la calculadora.
* **FormatString**: Este módulo contiene un procedimiento llamado `Format` que se utiliza para formatear números como cadenas.

El módulo `CalculadoraCompleja` contiene un procedimiento llamado `CalculaComplejo` que se utiliza para calcular el resultado de una operación compleja. Este procedimiento toma tres parámetros:

* **c1** y **c2**: Los dos números complejos que se van a operar.
* **signo**: El operador que se va a utilizar.

El procedimiento `CalculaComplejo` devuelve el resultado de la operación en el parámetro `resultado`.

El módulo `FormatString` contiene un procedimiento llamado `Format` que se utiliza para formatear números como cadenas. Este procedimiento toma tres parámetros:

* **formato**: El formato de la cadena que se va a generar.
* **valor**: El valor que se va a formatear.
* **cadena**: La cadena en la que se va a almacenar el valor formateado.

El procedimiento `Format` devuelve el número de caracteres que se han escrito en la cadena.

El código principal de la calculadora está contenido en el procedimiento `CalculadoraCompleja`. Este procedimiento solicita al usuario que introduzca dos números complejos y el operador que se va a utilizar. A continuación, llama al procedimiento `CalculaComplejo` para calcular el resultado de la operación. El resultado se muestra en la pantalla.

La calculadora continúa solicitando al usuario que introduzca números complejos y operadores hasta que el usuario introduzca una cadena que no sea "S".