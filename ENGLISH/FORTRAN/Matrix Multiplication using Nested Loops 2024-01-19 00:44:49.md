```fortran
program complex_code

implicit none

integer, parameter :: n = 100

real(kind=8), dimension(n, n) :: a, b, c

! Initialize matrices a and b
do i = 1, n
  do j = 1, n
    a(i, j) = 2.0*i + 3.0*j
    b(i, j) = 4.0*i - 5.0*j
  end do
end do

! Compute matrix c = a * b
do i = 1, n
  do j = 1, n
    c(i, j) = 0.0
    do k = 1, n
      c(i, j) = c(i, j) + a(i, k) * b(k, j)
    end do
  end do
end do

! Print the result
do i = 1, n
  do j = 1, n
    write(*, '(f10.4)') c(i, j)
  end do
end do

end program complex_code
```

This code performs matrix multiplication of two matrices `a` and `b`, and stores the result in matrix `c`. The code is complex because it uses nested loops to perform the matrix multiplication. The outer loops iterate over the rows and columns of the matrices, while the inner loop iterates over the elements of the matrices. The code also uses the `implicit none` statement, which requires all variables to be explicitly declared.

The `real(kind=8)` statement specifies that the matrices `a`, `b`, and `c` will be stored using 8-byte floating-point numbers. This is necessary to ensure that the matrices can store the results of the matrix multiplication without losing precision.

The `do` statements are used to create the nested loops. The first `do` statement creates the outer loop, which iterates over the rows of the matrices. The second `do` statement creates the middle loop, which iterates over the columns of the matrices. The third `do` statement creates the inner loop, which iterates over the elements of the matrices.

The line `c(i, j) = c(i, j) + a(i, k) * b(k, j)` performs the matrix multiplication. This line adds the product of the element `a(i, k)` and the element `b(k, j)` to the element `c(i, j)`.

The line `write(*, '(f10.4)') c(i, j)` prints the element `c(i, j)` to the console. The `f10.4` format specifier specifies that the number should be printed with a field width of 10 and 4 decimal places.

The output of the program will be a matrix of numbers that are the result of multiplying the matrices `a` and `b`.