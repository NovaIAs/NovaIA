```fortran
program complex_fortran_code

implicit none

integer, parameter :: n = 1000000  ! Number of data points

real(kind=8), dimension(n) :: x, y, z  ! Data arrays

! Calculate some complex mathematical function
do i = 1, n
   x(i) = sin(i / 1000.0)
   y(i) = cos(i / 1000.0)
   z(i) = x(i) * y(i)
end do

! Perform some statistical analysis on the data
real(kind=8) :: mean_x, mean_y, mean_z, std_x, std_y, std_z, cov_xy, corr_xy
mean_x = sum(x) / n
mean_y = sum(y) / n
mean_z = sum(z) / n
std_x = sqrt(sum((x - mean_x)**2) / (n - 1))
std_y = sqrt(sum((y - mean_y)**2) / (n - 1))
std_z = sqrt(sum((z - mean_z)**2) / (n - 1))
cov_xy = sum((x - mean_x) * (y - mean_y)) / (n - 1)
corr_xy = cov_xy / (std_x * std_y)

! Print the results
print *, "Mean of x:", mean_x
print *, "Mean of y:", mean_y
print *, "Mean of z:", mean_z
print *, "Standard deviation of x:", std_x
print *, "Standard deviation of y:", std_y
print *, "Standard deviation of z:", std_z
print *, "Covariance of x and y:", cov_xy
print *, "Correlation coefficient of x and y:", corr_xy

end program complex_fortran_code
```

Explanation:

This Fortran code performs a complex mathematical calculation on a large dataset of 1 million data points. It calculates the sine and cosine of each data point, multiplies them together, and then performs statistical analysis on the resulting dataset. The statistical analysis includes calculating the mean, standard deviation, covariance, and correlation coefficient of the data. The results are then printed to the console.

Here is a more detailed explanation of the code:

* The `implicit none` statement at the beginning of the program tells the compiler that no implicit typing is allowed. This means that all variables must be explicitly declared with a type.
* The `integer, parameter :: n = 1000000` statement declares an integer parameter named `n` with a value of 1000000. This parameter is used to specify the number of data points in the dataset.
* The `real(kind=8), dimension(n) :: x, y, z` statement declares three real arrays named `x`, `y`, and `z`, each with a size of `n`. These arrays will be used to store the data.
* The `do i = 1, n` loop iterates over each data point in the dataset.
* Inside the loop, the `x(i) = sin(i / 1000.0)` statement calculates the sine of the current data point and stores it in the `x` array.
* The `y(i) = cos(i / 1000.0)` statement calculates the cosine of the current data point and stores it in the `y` array.
* The `z(i) = x(i) * y(i)` statement multiplies the sine and cosine values together and stores the result in the `z` array.
* The `mean_x = sum(x) / n` statement calculates the mean of the `x` array and stores it in the `mean_x` variable.
* The `mean_y = sum(y) / n` statement calculates the mean of the `y` array and stores it in the `mean_y` variable.
* The `mean_z = sum(z) / n` statement calculates the mean of the `z` array and stores it in the `mean_z` variable.
* The `std_x = sqrt(sum((x - mean_x)**2) / (n - 1))` statement calculates the standard deviation of the `x` array and stores it in the `std_x` variable.
* The `std_y = sqrt(sum((y - mean_y)**2) / (n - 1))` statement calculates the standard deviation of the `y` array and stores it in the `std_y` variable.
* The `std_z = sqrt(sum((z - mean_z)**2) / (n - 1))` statement calculates the standard deviation of the `z` array and stores it in the `std_z` variable.
* The `cov_xy = sum((x - mean_x) * (y - mean_y)) / (n - 1)` statement calculates the covariance of the `x` and `y` arrays and stores it in the `cov_xy` variable.
* The `corr_xy = cov_xy / (std_x * std_y)` statement calculates the correlation coefficient of the `x` and `y` arrays and stores it in the `corr_xy` variable.
* The `print *, "Mean of x:", mean_x` statement prints the mean of the `x` array to the console.
* The `print *, "Mean of y:", mean_y` statement prints the mean of the `y` array to the console.
* The `print *, "Mean of z:", mean_z` statement prints the mean of the `z` array to the console.
* The `print *, "Standard deviation of x:", std_x` statement prints the standard deviation of the `x` array to the console.
* The `print *, "Standard deviation of y:", std_y` statement prints the standard deviation of the `y` array to the console.
* The `print *, "Standard deviation of z:", std_z` statement prints the standard deviation of the `z` array to the console.
* The `print *, "Covariance of x and y:", cov_xy` statement prints the covariance of the `x` and `y` arrays to the console.
* The `print *, "Correlation coefficient of x and y:", corr_xy` statement prints the correlation coefficient of the `x` and `y` arrays to the console.