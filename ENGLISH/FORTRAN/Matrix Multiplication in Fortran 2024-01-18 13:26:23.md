```fortran
program complex_fortran
  implicit none

  integer, parameter :: n = 1000
  real :: a(n,n), b(n,n), c(n,n)

  ! Initialize matrices a and b
  do i = 1, n
    do j = 1, n
      a(i,j) = random_real()
      b(i,j) = random_real()
    end do
  end do

  ! Compute matrix multiplication of a and b in c
  do i = 1, n
    do j = 1, n
      c(i,j) = 0.0
      do k = 1, n
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do

  ! Print the result matrix c
  do i = 1, n
    do j = 1, n
      write(*,*) c(i,j)
    end do
  end do

end program complex_fortran
```

This code performs matrix multiplication of two n x n matrices a and b, and stores the result in matrix c. The code uses three nested loops to compute the matrix multiplication. The outer two loops iterate over the rows and columns of the result matrix c, and the innermost loop iterates over the elements of the product of row i of matrix a and column j of matrix b. The code also uses the random_real() function to initialize the matrices a and b with random real numbers.

The code is complex because it uses nested loops, and the innermost loop has a floating point multiplication and addition operation. The code is also differentiated because it uses the random_real() function to initialize the matrices a and b with random real numbers. This means that the result matrix c will be different every time the code is run.