```modula-2
MODULE Complejo;

FROM IO IMPORT Write, Writeln, Read, Readln;

TYPE
  Complejo = RECORD
    re, im: LONGCARD;
  END;

(* Conjuntos de aritmética compleja *)
PROCEDURE Conjuntar_suma(VAR c1, c2: Complejo);
VAR
  a, b: LONGCARD;
BEGIN
  a := c1.re + c2.re;
  b := c1.im + c2.im;
  c1.re := a;
  c1.im := b;
END Conjuntar_suma;

PROCEDURE Conjuntar_resta(VAR c1, c2: Complejo);
VAR
  a, b: LONGCARD;
BEGIN
  a := c1.re - c2.re;
  b := c1.im - c2.im;
  c1.re := a;
  c1.im := b;
END Conjuntar_resta;

PROCEDURE Conjuntar_multiplicación(VAR c1, c2: Complejo);
VAR
  a, b, c, d: LONGCARD;
BEGIN
  a := c1.re * c2.re;
  b := c1.re * c2.im;
  c := c1.im * c2.re;
  d := c1.im * c2.im;
  c1.re := a - d;
  c1.im := b + c;
END Conjuntar_multiplicación;

PROCEDURE Conjuntar_división(VAR c1, c2: Complejo);
VAR
  a, b, c, d: LONGCARD;
BEGIN
  a := c1.re * c2.re + c1.im * c2.im;
  b := c1.im * c2.re - c1.re * c2.im;
  c := c2.re * c2.re + c2.im * c2.im;
  d := c2.im * c2.re - c2.re * c2.im;
  c1.re := a / c;
  c1.im := b / d;
END Conjuntar_división;

(* Funciones auxiliares *)
PROCEDURE Conjuntar_imprimir(c: Complejo);
VAR
  s: ARRAY[0..7] OF CHAR;
BEGIN
  System.LongCardToStr(c.re, s, 0);
  Write(s);
  Write("+");
  System.LongCardToStr(c.im, s, 0);
  Write(s);
  Writeln("i");
END Conjuntar_imprimir;

PROCEDURE Conjuntar_leer(VAR c: Complejo);
VAR
  s: ARRAY[0..7] OF CHAR;
BEGIN
  Readln(s);
  c.re := System.StrToLongCard(s);
  Readln(s);
  c.im := System.StrToLongCard(s);
END Conjuntar_leer;

(* Programa principal *)
VAR
  c1, c2: Complejo;

BEGIN
  Write("Introduzca el primer número complejo (re, im): ");
  Conjuntar_leer(c1);
  Write("Introduzca el segundo número complejo (re, im): ");
  Conjuntar_leer(c2);
  Writeln("La suma de los números complejos es:");
  Conjuntar_imprimir(c1);
  Conjuntar_suma(c1, c2);
  Conjuntar_imprimir(c1);
  Writeln("La resta de los números complejos es:");
  Conjuntar_imprimir(c1);
  Conjuntar_resta(c1, c2);
  Conjuntar_imprimir(c1);
  Writeln("La multiplicación de los números complejos es:");
  Conjuntar_imprimir(c1);
  Conjuntar_multiplicación(c1, c2);
  Conjuntar_imprimir(c1);
  Writeln("La división de los números complejos es:");
  Conjuntar_imprimir(c1);
  Conjuntar_división(c1, c2);
  Conjuntar_imprimir(c1);
END Complejo.
```

Explicación del código:

* El código define un tipo de registro llamado `Complejo` que contiene dos campos: `re` y `im`, ambos de tipo `LONGCARD`. Estos campos representan las partes real e imaginaria de un número complejo.
* El código define varios procedimientos para realizar operaciones aritméticas con números complejos:
    * `Conjuntar_suma()` suma dos números complejos.
    * `Conjuntar_resta()` resta dos números complejos.
    * `Conjuntar_multiplicación()` multiplica dos números complejos.
    * `Conjuntar_división()` divide dos números complejos.
* El código también define dos procedimientos auxiliares:
    * `Conjuntar_imprimir()` imprime un número complejo en la consola.
    * `Conjuntar_leer()` lee un número complejo de la consola.
* El programa principal solicita al usuario que introduzca dos números complejos y luego utiliza los procedimientos definidos anteriormente para realizar operaciones aritméticas con ellos. Los resultados de las operaciones se muestran en la consola.