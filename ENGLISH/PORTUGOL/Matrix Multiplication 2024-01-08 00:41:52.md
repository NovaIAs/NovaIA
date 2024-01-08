```portugol
program matriz;
type Matriz3x3 = array[1..3,1..3] of integer;
var
  A, B, C : Matriz3x3;
  i, j, k : integer;

begin
  for i := 1 to 3 do
    for j := 1 to 3 do
      read(A[i,j]);

  for i := 1 to 3 do
    for j := 1 to 3 do
      read(B[i,j]);

  for i := 1 to 3 do
    for j := 1 to 3 do
      C[i,j] := 0;

  for i := 1 to 3 do
    for j := 1 to 3 do
      for k := 1 to 3 do
        C[i,j] := C[i,j] + A[i,k] * B[k,j];

  for i := 1 to 3 do
    for j := 1 to 3 do
      write(C[i,j]:3);

  writeln;
end.
```

This code performs the multiplication of two matrices of size 3x3 and stores the result in a third matrix.

The code first declares the type `Matriz3x3`, which is an array of 3x3 integers.

Then, it declares the variables `A`, `B`, `C`, `i`, `j`, and `k`, which are used to store the matrices and the loop indices.

The code then reads the values of the matrices `A` and `B` from the standard input.

Next, it initializes the matrix `C` to all zeros.

Then, it performs the matrix multiplication using three nested loops.

Finally, it writes the values of the matrix `C` to the standard output.