```modula-2
MODULE ComplexDiffCode;
FROM FormatString IMPORT FormatInt;

PROCEDURE PrintToConsole(i, j: CARDINAL);
VAR
  buf: ARRAY[0..20] OF CHAR;
BEGIN
  FormatInt(buf, i);
  WriteString(buf + 1);
  WriteString(" + ");
  FormatInt(buf, j);
  WriteString(buf + 1);
  WriteString("i");
  WriteLn;
END PrintToConsole;

PROCEDURE PrintComplexNum(num: ARRAY OF CARDINAL);
VAR
  i: CARDINAL;
BEGIN
  FOR i := LBOUND num TO HBOUND num DO
    PrintToConsole(num[i], num[i+1]);
END PrintComplexNum;

PROCEDURE ComplexAdd(num1, num2: ARRAY OF CARDINAL): ARRAY OF CARDINAL;
VAR
  result: ARRAY[0..1] OF CARDINAL;
BEGIN
  result[0] := num1[0] + num2[0];
  result[1] := num1[1] + num2[1];
  RETURN result;
END ComplexAdd;

PROCEDURE ComplexMult(num1, num2: ARRAY OF CARDINAL): ARRAY OF CARDINAL;
VAR
  result: ARRAY[0..1] OF CARDINAL;
BEGIN
  result[0] := num1[0] * num2[0] - num1[1] * num2[1];
  result[1] := num1[0] * num2[1] + num1[1] * num2[0];
  RETURN result;
END ComplexMult;

PROCEDURE ComplexPow(num: ARRAY OF CARDINAL; power: CARDINAL): ARRAY OF CARDINAL;
VAR
  result: ARRAY[0..1] OF CARDINAL;
  i: CARDINAL;
BEGIN
  result[0] := 1;
  result[1] := 0;
  FOR i := 1 TO power DO
    result := ComplexMult(result, num);
  END;
  RETURN result;
END ComplexPow;

PROCEDURE ComplexNthRoots(num: ARRAY OF CARDINAL; n: CARDINAL): ARRAY OF ARRAY[0..1] OF CARDINAL;
VAR
  i, j: CARDINAL;
  theta: REAL;
  results: ARRAY[0..n-1] OF ARRAY[0..1] OF CARDINAL;
BEGIN
  theta := 2.0 * PI / n;
  FOR i := 0 TO n-1 DO
    results[i][0] := num[0] * COS(i * theta) + num[1] * SIN(i * theta);
    results[i][1] := num[0] * SIN(i * theta) - num[1] * COS(i * theta);
  END;
  RETURN results;
END ComplexNthRoots;

PROCEDURE ComplexPlot(num: ARRAY OF CARDINAL; xmin, xmax, ymin, ymax: REAL);
VAR
  x, y: REAL;
  i, j: CARDINAL;
  buf: ARRAY[0..20] OF CHAR;
BEGIN
  WriteString("Complex Plot:");
  WriteLn;
  FOR x := xmin TO xmax BY (xmax - xmin) / 80 DO
    FOR y := ymin TO ymax BY (ymax - ymin) / 24 DO
      i := ROUND(x);
      j := ROUND(y);
      FormatInt(buf, j);
      WriteString(buf + 1);
      WriteString(": ");
      IF num[0] * i + num[1] * j < 0 THEN
        Write("-");
      END;
      FormatInt(buf, ABS(num[0] * i + num[1] * j));
      WriteString(buf + 1);
      WriteLn;
    END;
  END;
END ComplexPlot;

PROCEDURE ComplexDemo;
VAR
  num1, num2, result: ARRAY[0..1] OF CARDINAL;
  i: CARDINAL;
BEGIN
  num1[0] := 3;
  num1[1] := 4;
  num2[0] := 5;
  num2[1] := 6;

  WriteString("Complex Number 1: ");
  PrintComplexNum(num1);

  WriteString("Complex Number 2: ");
  PrintComplexNum(num2);

  result := ComplexAdd(num1, num2);
  WriteString("Addition Result: ");
  PrintComplexNum(result);

  result := ComplexMult(num1, num2);
  WriteString("Multiplication Result: ");
  PrintComplexNum(result);

  result := ComplexPow(num1, 3);
  WriteString("Power Result (num1^3): ");
  PrintComplexNum(result);

  WriteString("5th Roots of num1:");
  WriteLn;
  PrintComplexNum(ComplexNthRoots(num1, 5));

  ComplexPlot(num1, -10.0, 10.0, -10.0, 10.0);
END ComplexDemo;

BEGIN
  ComplexDemo;
END ComplexDiffCode.
```

This code demonstrates complex number operations and visualizations in MODULA-2. Here's a breakdown of the code:

1. **Module Declaration:**
   - The code starts with the `MODULE ComplexDiffCode` declaration, which defines a new module for the complex number operations and visualization.

2. **Procedure `PrintToConsole`:**
   - This procedure prints two integers to the console, separated by a plus sign and the letter "i" (indicating the imaginary component).

3. **Procedure `PrintComplexNum`:**
   - This procedure takes an array of two integers (representing the real and imaginary parts of a complex number) and prints it to the console using the `PrintToConsole` procedure.

4. **Procedure `ComplexAdd`:**
   - This procedure takes two arrays of two integers (representing complex numbers) and returns a new array containing the sum of the two complex numbers.

5. **Procedure `ComplexMult`:**
   - This procedure takes two arrays of two integers (representing complex numbers) and returns a new array containing the product of the two complex numbers.

6. **Procedure `ComplexPow`:**
   - This procedure takes an array of two integers (representing a complex number) and an integer (representing the power to raise the complex number to) and returns a new array containing the result of raising the complex number to the given power.

7. **Procedure `ComplexNthRoots`:**
   - This procedure takes an array of two integers (representing a complex number) and an integer (representing the number of roots to find) and returns an array of arrays, each containing two integers and representing a root of the complex number.

8. **Procedure `ComplexPlot`:**
   - This procedure takes an array of two integers (representing a complex number), four real numbers (representing the minimum and maximum values for the x and y axes), and generates a text-based plot of the complex number on the specified range of the x and y axes.

9. **Procedure `ComplexDemo`:**
   - This procedure demonstrates the use of the complex number operations and visualization procedures by performing various operations on complex numbers and displaying the results.

10. **Main Program:**
    - The main program calls the `ComplexDemo` procedure to run the demonstration of complex number operations and visualization.

This code showcases complex number operations, including addition, multiplication, exponentiation, finding roots, and visualizing complex numbers on a text-based plot. It provides a comprehensive demonstration of complex number operations and visualizations in MODULA-2.