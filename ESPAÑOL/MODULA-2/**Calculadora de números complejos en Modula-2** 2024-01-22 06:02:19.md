```modula-2
MODULE Calculadora;
FROM Terminal IMPORT ReadChar;
FROM TextIo IMPORT WriteString, WriteLn, ReadLn;
FROM Formats IMPORT Format;

TYPE Complex = RECORD
  real, imag: LONGINT;
END;

PROCEDURE Sum(VAR x, y: Complex): Complex;
BEGIN
  x.real:=x.real+y.real; x.imag:=x.imag+y.imag;
  RETURN x;
END Sum;

PROCEDURE Diff(VAR x, y: Complex): Complex;
BEGIN
  x.real:=x.real-y.real; x.imag:=x.imag-y.imag;
  RETURN x;
END Diff;

PROCEDURE Mult(VAR x, y: Complex): Complex;
BEGIN
  VAR tmp: LONGINT;
  tmp:=x.real*y.real-x.imag*y.imag;
  x.imag:=x.real*y.imag+x.imag*y.real;
  x.real:=tmp;
  RETURN x;
END Mult;

PROCEDURE Div(VAR x, y: Complex): Complex;
VAR tmp: Complex;
BEGIN
  IF y.real=0 AND y.imag=0 THEN
    WriteString("División por cero\n");
    RETURN x;
  END;
  tmp:=Mult(x,Sum(y,-y));
  tmp:=tmp*(1/y.real);
  RETURN tmp;
END Div;

PROCEDURE ReadComplex(VAR x: Complex);
VAR s: STRING;
BEGIN
  WHILE NOT (ReadLn(s)) DO
    WriteString("Lectura errónea. Inténtalo de nuevo.\n");
  END;
  Format(s, "%ld %ld", x.real, x.imag);
END ReadComplex;

PROCEDURE WriteComplex(x: Complex);
BEGIN
  WriteString(Format("%ld+%ldi", x.real, x.imag));
END WriteComplex;

PROCEDURE Modulo(x: Complex): REAL;
BEGIN
  RETURN Sqrt(Sqr(x.real)+Sqr(x.imag));
END Modulo;

VAR x, y: Complex;
BEGIN
  WriteString("Complejo 1:\n"); ReadComplex(x);
  WriteString("Complejo 2:\n"); ReadComplex(y);
  WriteString("Suma:     "); WriteComplex(Sum(x,y)); WriteLn("");
  WriteString("Resta:    "); WriteComplex(Diff(x,y)); WriteLn("");
  WriteString("Producto: "); WriteComplex(Mult(x,y)); WriteLn("");
  WriteString("Cociente: "); WriteComplex(Div(x,y)); WriteLn("");
  Format(WriteLn("Módulo:   %f\n", Modulo(x)));
END Calculadora.
```

Explicación del código:

- Se define un tipo de datos complejo con dos campos, `real` e `imag`, que representan la parte real y la imaginaria de un número complejo, respectivamente.
- Se definen cuatro procedimientos que realizan operaciones aritméticas con números complejos: suma, resta, multiplicación y división.
- Se define un procedimiento para leer un número complejo desde la entrada estándar.
- Se define un procedimiento para escribir un número complejo en la salida estándar.
- Se define un procedimiento para calcular el módulo de un número complejo.
- Se declaran dos variables de tipo `Complex` llamadas `x` e `y`.
- Se solicita al usuario que introduzca dos números complejos.
- Se utilizan los procedimientos definidos anteriormente para realizar las operaciones aritméticas con los números complejos introducidos por el usuario.
- Se utiliza el procedimiento `WriteComplex` para escribir los resultados de las operaciones aritméticas en la salida estándar.
- Se utiliza el procedimiento `Modulo` para calcular el módulo de los números complejos introducidos por el usuario.
- Se utiliza el procedimiento `Format` para escribir el módulo de los números complejos en la salida estándar.