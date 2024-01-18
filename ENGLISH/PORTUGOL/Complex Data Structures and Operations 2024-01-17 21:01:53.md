```portugal
program ComplexCode;

const
  MAX_SIZE = 100;

type
  Array = array[0..MAX_SIZE - 1] of integer;
  Record = record
    field1: integer;
    field2: real;
    field3: Array;
  end;

var
  array1, array2: Array;
  record1: Record;

procedure InitializeArrays;
begin
  for i := 0 to MAX_SIZE - 1 do
  begin
    array1[i] := random(100);
    array2[i] := random(100.0);
  end;
end;

procedure InitializeRecord;
begin
  record1.field1 := random(100);
  record1.field2 := random(100.0);
  for i := 0 to MAX_SIZE - 1 do
    record1.field3[i] := random(100);
end;

procedure PrintArrays;
begin
  for i := 0 to MAX_SIZE - 1 do
    write(array1[i]:3, ' ');
  writeln;
  for i := 0 to MAX_SIZE - 1 do
    write(array2[i]:6.2, ' ');
  writeln;
end;

procedure PrintRecord;
begin
  write(record1.field1:3, ' ');
  write(record1.field2:6.2, ' ');
  for i := 0 to MAX_SIZE - 1 do
    write(record1.field3[i]:3, ' ');
  writeln;
end;

begin
  InitializeArrays;
  InitializeRecord;
  PrintArrays;
  PrintRecord;
end.
```

Explanation:

- The program first defines a constant `MAX_SIZE`, which is used to determine the size of the arrays and the record.
- It then defines three types: `Array`, `Record`, and `ComplexCode`.
- The `Array` type is a one-dimensional array of integers, and the `Record` type is a record that contains three fields: an integer, a real number, and an array of integers.
- The `ComplexCode` type is a procedure that takes no parameters and returns nothing.
- The program then declares three variables: `array1` and `array2`, which are both arrays of integers, and `record1`, which is a record of type `Record`.
- The `InitializeArrays` procedure is then called to initialize the values of the two arrays.
- The `InitializeRecord` procedure is then called to initialize the values of the record.
- The `PrintArrays` procedure is then called to print the values of the two arrays to the console.
- The `PrintRecord` procedure is then called to print the values of the record to the console.
- Finally, the `ComplexCode` procedure is called, which calls the `InitializeArrays`, `InitializeRecord`, `PrintArrays`, and `PrintRecord` procedures in sequence.