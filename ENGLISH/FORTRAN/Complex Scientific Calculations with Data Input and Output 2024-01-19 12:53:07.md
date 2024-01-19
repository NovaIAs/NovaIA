```fortran
program complex_code
  implicit none

  integer, parameter :: n = 100  ! Number of data points
  real, dimension(n) :: x, y, z  ! Arrays to store data

  ! Read data from a file
  open(unit=10, file='data.txt')
  read(10, *) x, y, z
  close(10)

  ! Perform some calculations on the data
  do i = 1, n
    z(i) = x(i) * y(i)
  end do

  ! Write the results to a file
  open(unit=20, file='results.txt')
  write(20, *) z
  close(20)

end program complex_code
```

Explanation:

* The program starts with the `program` statement, which declares the name of the program.
* The `implicit none` statement tells the compiler that no implicit typing is to be done. This means that every variable must be explicitly declared with its type.
* The `integer, parameter :: n = 100` statement declares an integer parameter named `n` with a value of 100. This parameter is used to specify the number of data points in the arrays `x`, `y`, and `z`.
* The `real, dimension(n) :: x, y, z` statement declares three arrays named `x`, `y`, and `z`. The arrays are all of type `real` and have a dimension of `n`.
* The `open(unit=10, file='data.txt')` statement opens a file named `data.txt` for reading.
* The `read(10, *) x, y, z` statement reads the data from the file and stores it in the arrays `x`, `y`, and `z`.
* The `close(10)` statement closes the file.
* The `do i = 1, n` statement starts a loop that iterates through the elements of the arrays `x`, `y`, and `z`.
* The `z(i) = x(i) * y(i)` statement calculates the value of the element `z(i)` as the product of the elements `x(i)` and `y(i)`.
* The `end do` statement ends the loop.
* The `open(unit=20, file='results.txt')` statement opens a file named `results.txt` for writing.
* The `write(20, *) z` statement writes the contents of the array `z` to the file.
* The `close(20)` statement closes the file.
* The `end program complex_code` statement ends the program.