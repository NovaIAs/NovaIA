```portugal
PROGRAM complex_code;

CONST
  MAX_SIZE = 100;

TYPE
  data_type = ARRAY [0..MAX_SIZE] OF INTEGER;

VAR
  data: data_type;
  i, j, k: INTEGER;

PROCEDURE fill_array(VAR a: data_type);
BEGIN
  FOR i := 0 TO MAX_SIZE DO
    a[i] := RANDOM(1000);
END;

PROCEDURE sort_array(VAR a: data_type);
VAR
  temp: INTEGER;
BEGIN
  FOR i := 0 TO MAX_SIZE - 1 DO
    FOR j := i + 1 TO MAX_SIZE DO
      IF a[j] < a[i] THEN
        temp := a[i];
        a[i] := a[j];
        a[j] := temp;
END;

PROCEDURE print_array(a: data_type);
VAR
  i: INTEGER;
BEGIN
  FOR i := 0 TO MAX_SIZE - 1 DO
    WRITE(a[i], ' ');
  WRITELN;
END;

BEGIN
  fill_array(data);
  sort_array(data);
  print_array(data);
END.
```

This code is a complex PORTUGOL program that does the following:

1. It defines a constant `MAX_SIZE` with a value of 100.
2. It defines a data type `data_type` as an array of integers with a maximum size of `MAX_SIZE`.
3. It declares several variables:
   - `data` is an array of integers of size `MAX_SIZE`.
   - `i`, `j`, and `k` are integer variables.
4. It defines a procedure `fill_array` that takes an array of integers as an argument and fills it with random integers between 0 and 999.
5. It defines a procedure `sort_array` that takes an array of integers as an argument and sorts it in ascending order.
6. It defines a procedure `print_array` that takes an array of integers as an argument and prints it to the console.
7. In the main program, it calls the `fill_array` procedure to fill the `data` array with random integers.
8. It calls the `sort_array` procedure to sort the `data` array in ascending order.
9. It calls the `print_array` procedure to print the `data` array to the console.

This code demonstrates several programming concepts, including:

- Defining constants, data types, and variables.
- Writing procedures with parameters.
- Using arrays to store data.
- Using loops to iterate over data.
- Sorting data in ascending order.
- Printing data to the console.