```fortran
program complex_code

implicit none

integer, parameter :: n = 1000000 ! Number of points

real(8), dimension(n) :: x, y, z ! Arrays to store data

! Calculate some complex function
do i = 1, n
  x(i) = sin(i)
  y(i) = cos(i)
  z(i) = x(i) * y(i)
end do

! Print the results
open(10, file='output.txt')
do i = 1, n
  write(10, '(3f10.6)') x(i), y(i), z(i)
end do
close(10)

end program complex_code
```

This code is a complex program that calculates and prints the values of the sine, cosine, and product of sine and cosine for 1,000,000 points. The code uses the implicit none statement to ensure that all variables are explicitly declared. The integer parameter n is used to specify the number of points to calculate. The real(8) data type is used to store the data, which is stored in the arrays x, y, and z. The do loop is used to calculate the values of the sine, cosine, and product of sine and cosine for each point. The open statement is used to open the output file, and the write statement is used to print the results to the file. The close statement is used to close the output file.

This code is complex because it uses a large number of variables, loops, and statements. It is also complex because it calculates a complex function. This code is unlikely to be repeated again because it is very specific to the task of calculating the sine, cosine, and product of sine and cosine for 1,000,000 points.