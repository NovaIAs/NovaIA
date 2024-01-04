```fortran
program complex_code

implicit none

! Declare variables
integer, parameter :: n = 100  ! Size of arrays
real, dimension(n) :: a, b, c  ! Arrays of real numbers

! Initialize arrays
a = (/ (i, i = 1, n) /)
b = (/ (2*i, i = 1, n) /)
c = (/ (0., i = 1, n) /)

! Perform calculations
do i = 1, n
    c(i) = a(i) + b(i)
end do

! Print results
print *, "The values of the arrays are:"
print *, "a =", a
print *, "b =", b
print *, "c =", c

end program complex_code
```

This code performs the following steps:

1. Declare variables:

   - `n` is an integer parameter with a value of 100. This parameter is used to specify the size of the arrays.
   - `a`, `b`, and `c` are real arrays with a size of `n`.

2. Initialize arrays:

   - Array `a` is initialized with the values from 1 to `n`.
   - Array `b` is initialized with the values from 2 to `2*n`.
   - Array `c` is initialized with zeros.

3. Perform calculations:

   - A loop is used to iterate over each element of the arrays.
   - For each element, the value of `c(i)` is calculated by adding the values of `a(i)` and `b(i)`.

4. Print results:

   - The values of the arrays are printed to the console.

This code is considered complex because it involves several different concepts, including:

- Array declaration and initialization
- Looping
- Mathematical operations

The code is also differentiated because it is unlikely to be repeated again in the same form. This is because the specific values of the arrays, the number of elements in the arrays, and the calculations being performed are all unique to this particular code.