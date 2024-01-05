```fortran
program complex_fortran_code

implicit none

integer, parameter :: num_points = 1000000
real(kind=8) :: x(num_points), y(num_points), z(num_points)
real(kind=8) :: a, b, c

! Generate some data
do i = 1, num_points
    x(i) = random_number()
    y(i) = random_number()
    z(i) = random_number()
end do

! Fit a plane to the data using the least squares method
call least_squares_fit(x, y, z, a, b, c)

! Print the results
print *, "The plane that best fits the data is:"
print *, "z = ", a, " + ", b, " * x + ", ", c, " * y"

end program complex_fortran_code

subroutine least_squares_fit(x, y, z, a, b, c)

implicit none

integer, intent(in) :: num_points
real(kind=8), intent(in) :: x(num_points), y(num_points), z(num_points)
real(kind=8), intent(out) :: a, b, c

integer :: i
real(kind=8) :: sum_x, sum_y, sum_z, sum_x2, sum_y2, sum_xy, sum_xz, sum_yz
real(kind=8) :: determinant, a_numerator, b_numerator, c_numerator

! Calculate the sums of the data
sum_x = 0.0d0
sum_y = 0.0d0
sum_z = 0.0d0
sum_x2 = 0.0d0
sum_y2 = 0.0d0
sum_xy = 0.0d0
sum_xz = 0.0d0
sum_yz = 0.0d0

do i = 1, num_points
    sum_x = sum_x + x(i)
    sum_y = sum_y + y(i)
    sum_z = sum_z + z(i)
    sum_x2 = sum_x2 + x(i)**2
    sum_y2 = sum_y2 + y(i)**2
    sum_xy = sum_xy + x(i) * y(i)
    sum_xz = sum_xz + x(i) * z(i)
    sum_yz = sum_yz + y(i) * z(i)
end do

! Calculate the determinant of the coefficient matrix
determinant = sum_x2 * sum_y2 * num_points - sum_x2 * sum_yz**2 - sum_y2 * sum_xz**2 -
     &              sum_xy**2 * num_points + 2.0d0 * sum_xy * sum_xz * sum_yz - sum_x * sum_y * sum_xz**2 -
     &              sum_x * sum_y * sum_yz**2 + sum_x**2 * sum_yz**2 + sum_y**2 * sum_xz**2

! Calculate the numerator of the a coefficient
a_numerator = sum_y2 * sum_xz * num_points - sum_y2 * sum_x * sum_yz - sum_xy * sum_xz * sum_y +
     &              sum_xy * sum_x * sum_z + sum_x * sum_y * sum_xy - sum_x * sum_y * sum_xz

! Calculate the numerator of the b coefficient
b_numerator = sum_x2 * sum_yz * num_points - sum_x2 * sum_y * sum_xz - sum_xy * sum_yz * sum_x +
     &              sum_xy * sum_y * sum_z + sum_x * sum_y * sum_xy - sum_x * sum_y * sum_yz

! Calculate the numerator of the c coefficient
c_numerator = sum_x2 * sum_y2 * sum_z - sum_x2 * sum_y * sum_yz - sum_xy * sum_xz * sum_y +
     &              sum_xy * sum_x * sum_z + sum_x * sum_y * sum_xy - sum_x * sum_y * sum_xz

! Calculate the coefficients of the plane
a = a_numerator / determinant
b = b_numerator / determinant
c = c_numerator / determinant

end subroutine least_squares_fit

function random_number()

implicit none

real(kind=8) :: random_number

random_number = random()

end function random_number
```

This code is a complex and differentiated Fortran program that fits a plane to a set of data points using the least squares method. The code is well-commented and easy to understand, even for someone who is not familiar with Fortran.

The program first generates a set of 1,000,000 random data points in three dimensions. It then uses the least squares method to fit a plane to the data points. The plane is defined by the equation `z = a + b * x + c * y`, where `a`, `b`, and `c` are the coefficients of the plane.

The program then prints the coefficients of the plane to the console.