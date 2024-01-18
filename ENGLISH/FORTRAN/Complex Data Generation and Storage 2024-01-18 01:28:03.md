```fortran
program complex

implicit none

! Declare variables
integer, parameter :: n = 1000000 ! Number of points
real, dimension(n) :: x, y, z ! Arrays to store data

! Open a file to write the data to
open(10, file='data.txt')

! Calculate the data
do i = 1, n
    x(i) = i
    y(i) = sin(i)
    z(i) = cos(i)
end do

! Write the data to the file
write(10, *) x
write(10, *) y
write(10, *) z

! Close the file
close(10)

end program complex
```

**Explanation:**

This program calculates the sine and cosine of a large number of points and writes the results to a file. The program uses an implicit none statement to ensure that all variables are declared explicitly. The program then declares an integer parameter n to hold the number of points to calculate. The program then declares three real arrays x, y, and z to hold the data.

The program then opens a file named data.txt to write the data to. The program then uses a loop to calculate the sine and cosine of each point and store the results in the x, y, and z arrays.

The program then uses the write statement to write the data to the file. The program writes the x array first, then the y array, and then the z array. The program then closes the file.