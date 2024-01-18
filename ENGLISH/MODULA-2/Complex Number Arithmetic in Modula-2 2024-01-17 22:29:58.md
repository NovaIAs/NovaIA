```modula-2
MODULE ComplexModula2Code;
FROM FormatString IMPORT Format;
FROM Terminal IMPORT WriteString, WriteLn;

TYPE
  Complex = RECORD
    real: REAL;
    imag: REAL;
  END;

PROCEDURE Add(a, b: Complex): Complex;
BEGIN
  RETURN Complex(a.real + b.real, a.imag + b.imag);
END Add;

PROCEDURE Subtract(a, b: Complex): Complex;
BEGIN
  RETURN Complex(a.real - b.real, a.imag - b.imag);
END Subtract;

PROCEDURE Multiply(a, b: Complex): Complex;
VAR
  temp: REAL;
BEGIN
  temp := a.real * b.real - a.imag * b.imag;
  a.imag := a.real * b.imag + a.imag * b.real;
  a.real := temp;
  RETURN a;
END Multiply;

PROCEDURE Divide(a, b: Complex): Complex;
VAR
  temp: REAL;
BEGIN
  temp := (a.real * b.real + a.imag * b.imag) / (b.real * b.real + b.imag * b.imag);
  a.imag := (a.imag * b.real - a.real * b.imag) / (b.real * b.real + b.imag * b.imag);
  a.real := temp;
  RETURN a;
END Divide;

PROCEDURE PrintComplex(c: Complex);
VAR
  s: ARRAY[1..25] OF CHAR;
BEGIN
  Format(s, "%.2f + %.2fi", [c.real, c.imag]);
  WriteString(s);
END PrintComplex;

VAR
  a, b, c: Complex;

BEGIN
  a := Complex(1.0, 2.0);
  b := Complex(3.0, 4.0);

  c := Add(a, b);
  WriteString("Addition: ");
  PrintComplex(c);
  WriteLn;

  c := Subtract(a, b);
  WriteString("Subtraction: ");
  PrintComplex(c);
  WriteLn;

  c := Multiply(a, b);
  WriteString("Multiplication: ");
  PrintComplex(c);
  WriteLn;

  c := Divide(a, b);
  WriteString("Division: ");
  PrintComplex(c);
  WriteLn;
END ComplexModula2Code.
```

Explanation:

- The code defines a module ComplexModula2Code, which contains the implementation of complex numbers in Modula-2.

- It starts by defining a record type Complex, which represents complex numbers. It has two fields, real and imag, to store the real and imaginary parts of the complex number, respectively.

- Next, it defines four procedures: Add, Subtract, Multiply, and Divide, which perform the basic arithmetic operations on complex numbers. These procedures take two complex numbers as arguments and return a new complex number as the result of the operation.

- The PrintComplex procedure is used to print a complex number in a human-readable format. It uses the FormatString module to format the output string.

- Finally, the code has a main block, which creates two complex numbers a and b, and then uses the arithmetic procedures to perform various operations on them. The results are printed to the console.