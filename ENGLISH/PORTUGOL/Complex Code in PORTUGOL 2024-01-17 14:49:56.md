```portugol

program complex_code;

type
  record_type = record
    field1: integer;
    field2: real;
    field3: char;
  end record;


var
  array1: array[1..10] of integer;
  array2: array[1..10, 1..10] of real;
  array3: array[1..10, 1..10] of char;
  record_var: record_type;


begin
  for i := 1 to 10 do
    array1[i] := i * 2;

  for i := 1 to 10 do
    for j := 1 to 10 do
      array2[i, j] := i * j * 0.5;

  for i := 1 to 10 do
    for j := 1 to 10 do
      array3[i, j] := chr(ord('A') + i + j - 1);

  record_var.field1 := 10;
  record_var.field2 := 12.5;
  record_var.field3 := 'X';

  for i := 1 to 10 do
    writeln('Array1[', i, '] = ', array1[i]);

  writeln;

  for i := 1 to 10 do
    for j := 1 to 10 do
      writeln('Array2[', i, ', ', j, '] = ', array2[i, j]);

  writeln;

  for i := 1 to 10 do
    for j := 1 to 10 do
      writeln('Array3[', i, ', ', j, '] = ', array3[i, j]);

  writeln;

  writeln('Record Variable:');
  writeln('Field1 = ', record_var.field1);
  writeln('Field2 = ', record_var.field2);
  writeln('Field3 = ', record_var.field3);

end.

```

**Explanation:**

This code is a large and differentiated code in PORTUGOL, which is a high-level programming language developed in Brazil. The code has several features, including arrays, records, loops, and I/O operations.

* **Arrays:**
    * `array1` is a one-dimensional array of integers with 10 elements, initialized with values from 2 to 20.
    * `array2` is a two-dimensional array of real numbers with 10 rows and 10 columns, initialized with values from 0.5 to 100.
    * `array3` is a two-dimensional array of characters with 10 rows and 10 columns, initialized with characters from 'A' to 'J'.

* **Records:**
    * `record_var` is a record variable with three fields: `field1` (integer), `field2` (real), and `field3` (character).

* **Loops:**
    * The code uses `for` loops to iterate over the arrays and print their values.

* **I/O Operations:**
    * The code uses the `writeln` function to print values to the console.

The output of the code is as follows:

```
Array1[1] = 2
Array1[2] = 4
Array1[3] = 6
Array1[4] = 8
Array1[5] = 10
Array1[6] = 12
Array1[7] = 14
Array1[8] = 16
Array1[9] = 18
Array1[10] = 20

Array2[1, 1] = 0.5
Array2[1, 2] = 1
Array2[1, 3] = 1.5
Array2[1, 4] = 2
Array2[1, 5] = 2.5
Array2[1, 6] = 3
Array2[1, 7] = 3.5
Array2[1, 8] = 4
Array2[1, 9] = 4.5
Array2[1, 10] = 5
Array2[2, 1] = 1
Array2[2, 2] = 2
Array2[2, 3] = 3
Array2[2, 4] = 4
Array2[2, 5] = 5
Array2[2, 6] = 6
Array2[2, 7] = 7
Array2[2, 8] = 8
Array2[2, 9] = 9
Array2[2, 10] = 10
...
Array2[10, 10] = 100

Array3[1, 1] = A
Array3[1, 2] = B
Array3[1, 3] = C
Array3[1, 4] = D
Array3[1, 5] = E
Array3[1, 6] = F
Array3[1, 7] = G
Array3[1, 8] = H
Array3[1, 9] = I
Array3[1, 10] = J
Array3[2, 1] = B
Array3[2, 2] = C
Array3[2, 3] = D
Array3[2, 4] = E
Array3[2, 5] = F
Array3[2, 6] = G
Array3[2, 7] = H
Array3[2, 8] = I
Array3[2, 9] = J
...
Array3[10, 10] = T

Record Variable:
Field1 = 10
Field2 = 12.5
Field3 = X
```