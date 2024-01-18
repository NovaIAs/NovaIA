```modula-2
MODULE ComplexCalculator;
FROM Terminal IMPORT WriteString, WriteLn, ReadLine;

PROCEDURE ComplexNumber(a, b: REAL) =
  RECORD
    real : REAL;
    imag : REAL;
  END;

PROCEDURE Add(a, b: ComplexNumber) : ComplexNumber;
(* Adds two complex numbers. *)
BEGIN
  ComplexNumber.real := a.real + b.real;
  ComplexNumber.imag := a.imag + b.imag;
  RETURN ComplexNumber;
END Add;

PROCEDURE Subtract(a, b: ComplexNumber) : ComplexNumber;
(* Subtracts two complex numbers. *)
BEGIN
  ComplexNumber.real := a.real - b.real;
  ComplexNumber.imag := a.imag - b.imag;
  RETURN ComplexNumber;
END Subtract;

PROCEDURE Multiply(a, b: ComplexNumber) : ComplexNumber;
(* Multiplies two complex numbers. *)
(* (a + bi)(c + di) = (ac - bd) + (ad + bc)i *)
BEGIN
  ComplexNumber.real := (a.real * b.real) - (a.imag * b.imag);
  ComplexNumber.imag := (a.real * b.imag) + (a.imag * b.real);
  RETURN ComplexNumber;
END Multiply;

PROCEDURE Divide(a, b: ComplexNumber) : ComplexNumber;
(* Divides two complex numbers. *)
(* (a + bi)/(c + di) = ((ac + bd)/(c^2 + d^2)) + ((bc - ad)/(c^2 + d^2))i *)
BEGIN
  IF b.real = 0.0 AND b.imag = 0.0 THEN
    WriteString("Error: Division by zero.");
    RETURN ComplexNumber;
  END;
  ComplexNumber.real := ((a.real * b.real) + (a.imag * b.imag)) /
    ((b.real * b.real) + (b.imag * b.imag));
  ComplexNumber.imag := ((b.real * a.imag) - (a.real * b.imag)) /
    ((b.real * b.real) + (b.imag * b.imag));
  RETURN ComplexNumber;
END Divide;

PROCEDURE PrintComplexNumber(a: ComplexNumber);
(* Prints a complex number to the terminal. *)
BEGIN
  WriteString(Float(a.real), 0);
  WriteString(" + ", 0);
  WriteString(Float(a.imag), 0);
  WriteString("i", 0);
END PrintComplexNumber;

VAR
  a, b: ComplexNumber;
BEGIN
  WriteString("Enter the first complex number (a + bi): ", 90);
  ReadLine(a.real);
  ReadLine(a.imag);
  WriteString("Enter the second complex number (c + di): ", 90);
  ReadLine(b.real);
  ReadLine(b.imag);

  WriteString("Sum: ", 5);
  PrintComplexNumber(Add(a, b));
  WriteLn;

  WriteString("Difference: ", 11);
  PrintComplexNumber(Subtract(a, b));
  WriteLn;

  WriteString("Product: ", 8);
  PrintComplexNumber(Multiply(a, b));
  WriteLn;

  WriteString("Quotient: ", 9);
  PrintComplexNumber(Divide(a, b));
  WriteLn;

END ComplexCalculator.
```

This code defines a module called `ComplexCalculator`, which provides a way to perform basic arithmetic operations on complex numbers. This program allows the user to enter two complex numbers in the form of a + bi and c + di, where a, b, c, and d are real numbers. Here's how the code works:

- It defines a record type called `ComplexNumber` with two fields `real` and `imag` to represent the real and imaginary parts of a complex number.

- It declares several procedures: `Add`, `Subtract`, `Multiply`, `Divide`, `PrintComplexNumber`, and the main program block.

- The `Add` procedure takes two complex numbers as input and returns a new complex number representing their sum.

- The `Subtract` procedure takes two complex numbers as input and returns a new complex number representing their difference.

- The `Multiply` procedure takes two complex numbers as input and returns a new complex number representing their product.

- The `Divide` procedure takes two complex numbers as input and returns a new complex number representing their quotient. If the second complex number is zero (i.e., division by zero), it prints an error message.

- The `PrintComplexNumber` procedure takes a complex number as input and prints it to the terminal in the form of a + bi.

- The main program block starts by prompting the user to enter the first and second complex numbers. It then calls the `Add`, `Subtract`, `Multiply`, and `Divide` procedures to perform the operations and display the results.