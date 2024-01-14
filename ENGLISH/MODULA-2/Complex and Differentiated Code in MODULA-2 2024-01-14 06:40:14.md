```modula-2
MODULE ComplexCode;
FROM Terminal IMPORT WriteString, WriteLn;

TYPE
  RecordType = RECORD
    Field1: INTEGER;
    Field2: BOOLEAN;
    Field3: STRING;
  END;

VAR
  SomeVariable: RecordType;
  ArrayVariable: ARRAY[1..10] OF INTEGER;
  ProcedureVariable: PROCEDURE (x: INTEGER): BOOLEAN;

PROCEDURE InitSomeVariable;
BEGIN
  SomeVariable.Field1 := 123;
  SomeVariable.Field2 := TRUE;
  SomeVariable.Field3 := "Hello, world!";
END InitSomeVariable;

PROCEDURE InitArrayVariable;
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO 10 DO
    ArrayVariable[i] := i * i;
  END;
END InitArrayVariable;

PROCEDURE InitProcedureVariable;
BEGIN
  ProcedureVariable := PROCEDURE (x: INTEGER): BOOLEAN
  VAR
    y: INTEGER;
  BEGIN
    y := x MOD 2;
    RETURN y = 0;
  END;
END InitProcedureVariable;

PROCEDURE PrintSomeVariable;
BEGIN
  WriteString("Field1: "); WriteLn(SomeVariable.Field1);
  WriteString("Field2: "); WriteLn(SomeVariable.Field2);
  WriteString("Field3: "); WriteLn(SomeVariable.Field3);
END PrintSomeVariable;

PROCEDURE PrintArrayVariable;
VAR
  i: INTEGER;
BEGIN
  FOR i := 1 TO 10 DO
    WriteString(ArrayVariable[i]);
    IF i < 10 THEN WriteString(", ");
  END;
  WriteLn;
END PrintArrayVariable;

PROCEDURE PrintProcedureVariable;
BEGIN
  IF ProcedureVariable(123) THEN
    WriteString("123 is even")
  ELSE
    WriteString("123 is odd")
  END;
  WriteLn;
END PrintProcedureVariable;

BEGIN
  InitSomeVariable;
  InitArrayVariable;
  InitProcedureVariable;

  PrintSomeVariable;
  PrintArrayVariable;
  PrintProcedureVariable;
END ComplexCode.
```

This code is a complex and differentiated code in MODULA-2. It defines a record type, an array variable, a procedure variable, and several procedures to initialize and print these variables. The code is written in English and is easy to understand.

Here is a breakdown of the code:

* The `RecordType` type defines a record with three fields: `Field1` (an integer), `Field2` (a boolean), and `Field3` (a string).
* The `SomeVariable` variable is declared as a `RecordType` variable.
* The `ArrayVariable` variable is declared as an array of 10 integers.
* The `ProcedureVariable` variable is declared as a procedure that takes an integer argument and returns a boolean value.
* The `InitSomeVariable` procedure initializes the `SomeVariable` variable with some values.
* The `InitArrayVariable` procedure initializes the `ArrayVariable` variable with some values.
* The `InitProcedureVariable` procedure initializes the `ProcedureVariable` variable with a procedure that checks if an integer is even.
* The `PrintSomeVariable` procedure prints the values of the `SomeVariable` variable.
* The `PrintArrayVariable` procedure prints the values of the `ArrayVariable` variable.
* The `PrintProcedureVariable` procedure prints the value of the `ProcedureVariable` variable.
* The `ComplexCode` module initializes the variables, prints their values, and then terminates.

This code is a good example of a complex and differentiated code in MODULA-2. It shows how to define and use different types of variables and procedures, and how to initialize and print their values.