**Programa en MODULA-2 para calcular el área de un triángulo:**

```modula-2
MODULE Triangulo;
FROM Text IMPORT WriteString, WriteLn;

PROCEDURE CalculaArea(a, b, c: LONGINT): LONGINT;
VAR
  s: LONGINT;
BEGIN
  s := (a + b + c) / 2;
  RETURN s * (s - a) * (s - b) * (s - c);
END CalculaArea;

VAR
  a, b, c: LONGINT;
BEGIN
  WriteString("Ingrese el lado a del triángulo: ");
  Read(a);
  WriteString("Ingrese el lado b del triángulo: ");
  Read(b);
  WriteString("Ingrese el lado c del triángulo: ");
  Read(c);
  WriteString("El área del triángulo es: ");
  WriteLn(CalculaArea(a, b, c));
END Triangulo.
```

**Explicación del código:**

* El programa comienza con el módulo `Triangulo`, que contiene todos los procedimientos y variables del programa.
* El procedimiento `CalculaArea` calcula el área de un triángulo dados sus tres lados.
* La variable `s` almacena el semiperímetro del triángulo.
* La expresión `s * (s - a) * (s - b) * (s - c)` calcula el área del triángulo utilizando la fórmula de Herón.
* La variable `a`, `b` y `c` almacenan los lados del triángulo.
* La instrucción `WriteString` muestra un mensaje en la pantalla.
* La instrucción `Read` lee un valor ingresado por el usuario.
* La instrucción `WriteLn` muestra un mensaje en la pantalla y salta una línea.
* La llamada al procedimiento `CalculaArea` calcula el área del triángulo y devuelve el resultado.
* La instrucción `WriteLn` muestra el área del triángulo en la pantalla.