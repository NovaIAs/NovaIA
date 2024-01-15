```fortran
program complex_code

implicit none

! Declare variables
integer, dimension(100) :: data
real, dimension(100) :: results

! Read data from a file
open(10, file='data.txt')
read(10, *) data

! Perform some complex calculations
do i = 1, 100
  results(i) = sin(data(i)) + cos(data(i))
end do

! Write results to a file
open(20, file='results.txt')
write(20, *) results

! End program
end program complex_code
```

**Explanation:**

1. The program begins with the `program` statement, which specifies the name of the program.

2. The `implicit none` statement is used to tell the compiler that no implicit typing is allowed. This means that every variable must be explicitly declared with a type.

3. The `integer, dimension(100)` statement declares an array of 100 integers named `data`.

4. The `real, dimension(100)` statement declares an array of 100 real numbers named `results`.

5. The `open(10, file='data.txt')` statement opens the file `data.txt` for reading.

6. The `read(10, *) data` statement reads the data from the file `data.txt` and stores it in the array `data`.

7. The `do i = 1, 100` statement starts a loop that will execute 100 times.

8. The `results(i) = sin(data(i)) + cos(data(i))` statement calculates the sine and cosine of each element in the array `data` and stores the results in the array `results`.

9. The `end do` statement ends the loop.

10. The `open(20, file='results.txt')` statement opens the file `results.txt` for writing.

11. The `write(20, *) results` statement writes the results from the array `results` to the file `results.txt`.

12. The `end program complex_code` statement ends the program.