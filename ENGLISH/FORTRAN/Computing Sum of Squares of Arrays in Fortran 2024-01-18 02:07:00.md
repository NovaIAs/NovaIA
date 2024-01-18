program complex_fortran
implicit none

integer, parameter :: n = 100000  ! Size of arrays
real(8), dimension(n) :: a, b, c  ! Arrays of doubles

! Initialize arrays
do i = 1, n
  a(i) = 1.0 / real(i)
  b(i) = 2.0 * a(i)
  c(i) = a(i) + b(i)
end do

! Compute sum of squares of each array
sum_a = sum(a**2)
sum_b = sum(b**2)
sum_c = sum(c**2)

! Print results
print *, "Sum of squares of array a:", sum_a
print *, "Sum of squares of array b:", sum_b
print *, "Sum of squares of array c:", sum_c

end program complex_fortran

! Explanation:

1. Program Structure:

The program begins with the "program" statement, which declares the starting point of the Fortran program. The "implicit none" statement specifies that no implicit data typing is allowed, which ensures that all variables are explicitly declared.

2. Data Declaration:

The program uses three arrays, "a," "b," and "c," each containing "n" double-precision (real(8)) elements. The "dimension" statement specifies the size of these arrays.

3. Array Initialization:

A "do loop" is used to initialize the elements of the arrays "a," "b," and "c." The loop iterates from 1 to "n" and assigns values to the array elements using mathematical expressions.

4. Array Summation:

The "sum" function is used to compute the sum of squares of each array. The "sum_a," "sum_b," and "sum_c" variables store the results.

5. Output:

The "print" statements are used to display the computed sums of squares on the console.

This code showcases advanced features of Fortran, such as array handling, loops, and mathematical operations. It demonstrates the flexibility and power of Fortran for scientific computing tasks.