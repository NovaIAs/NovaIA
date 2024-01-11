```fortran
program complex_code

implicit none

integer, parameter :: N = 1000000  ! Number of points to calculate
real(8), dimension(N) :: x, y, z    ! Arrays to store the data

! Calculate the data
do i = 1, N
    x(i) = sin(i / 100.0)
    y(i) = cos(i / 100.0)
    z(i) = tan(i / 100.0)
end do

! Find the maximum and minimum values
real(8) :: max_x, max_y, max_z, min_x, min_y, min_z
max_x = maxval(x)
max_y = maxval(y)
max_z = maxval(z)
min_x = minval(x)
min_y = minval(y)
min_z = minval(z)

! Print the results
print *, 'Maximum values:'
print *, 'x = ', max_x
print *, 'y = ', max_y
print *, 'z = ', max_z
print *, 'Minimum values:'
print *, 'x = ', min_x
print *, 'y = ', min_y
print *, 'z = ', min_z

end program complex_code
```

Explanation:

* The program calculates the sine, cosine, and tangent of a large number of points and stores the results in arrays `x`, `y`, and `z`.
* It then finds the maximum and minimum values of each array and prints the results.
* The program uses the `maxval` and `minval` functions to find the maximum and minimum values of the arrays.
* The `print *` statement is used to print the results to the console.