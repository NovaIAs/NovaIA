```fortran
program complex_code

implicit none

integer, parameter :: n = 1000 ! Number of data points

real, dimension(n) :: x, y, z ! Data arrays

! Generate some data
x = (/ (i, i = 1, n) /)
y = sin(x)
z = cos(x)

! Calculate some statistics
mean_x = sum(x) / n
mean_y = sum(y) / n
mean_z = sum(z) / n

std_dev_x = sqrt(sum((x - mean_x)**2) / (n - 1))
std_dev_y = sqrt(sum((y - mean_y)**2) / (n - 1))
std_dev_z = sqrt(sum((z - mean_z)**2) / (n - 1))

! Print the results
print *, 'Mean of x:', mean_x
print *, 'Mean of y:', mean_y
print *, 'Mean of z:', mean_z
print *, 'Standard deviation of x:', std_dev_x
print *, 'Standard deviation of y:', std_dev_y
print *, 'Standard deviation of z:', std_dev_z

end program complex_code
```

This code generates three arrays of data, `x`, `y`, and `z`, and then calculates the mean and standard deviation of each array. The code uses a variety of Fortran features, including:

* **Implicit typing:** The `implicit none` statement at the beginning of the program tells the compiler that all variables must be explicitly declared. This helps to catch errors early and makes the code more readable.
* **Parameter statements:** The `parameter` statement is used to define constants. In this code, the parameter `n` is used to specify the number of data points in the arrays.
* **Array declarations:** The `dimension` statement is used to declare arrays. In this code, the arrays `x`, `y`, and `z` are all declared to have `n` elements.
* **Data generation:** The `(/ (i, i = 1, n) /)` expression is used to generate the data for the `x` array. This expression creates a list of integers from 1 to `n`. The `sin` and `cos` functions are then used to generate the data for the `y` and `z` arrays, respectively.
* **Statistical calculations:** The `sum` and `sqrt` functions are used to calculate the mean and standard deviation of the data arrays. The `(x - mean_x)**2` expression is used to calculate the squared difference between each element of the `x` array and the mean of the `x` array. The `(n - 1)` expression is used to calculate the degrees of freedom for the standard deviation calculation.
* **Printing the results:** The `print` statement is used to print the results of the statistical calculations.

This code is a good example of how Fortran can be used to perform complex calculations on large datasets.