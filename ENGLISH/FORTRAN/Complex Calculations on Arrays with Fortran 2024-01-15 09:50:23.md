```fortran
program complex_code

implicit none

integer, parameter :: n = 10000 ! Size of arrays
real(kind=8) :: a(n), b(n), c(n) ! Declare arrays

! Initialize arrays with random numbers
call random_number(a)
call random_number(b)
call random_number(c)

! Perform complex calculations on arrays
do i = 1, n
  c(i) = a(i) + b(i) * sin(a(i)) + c(i) * cos(b(i))
end do

! Print results
print *, "Results:"
do i = 1, 10 ! Print first 10 elements
  print *, c(i)
end do

end program complex_code

subroutine random_number(array)

implicit none

real(kind=8), intent(out) :: array(:) ! Array to fill with random numbers

integer :: i

! Generate random numbers and fill array
do i = 1, size(array)
  array(i) = random_number()
end do

end subroutine random_number
```

Explanation:

1. Declare arrays `a`, `b`, and `c` with a size of `n` (10000 in this case). These arrays will hold real numbers.

2. Initialize the arrays with random numbers using the `random_number` subroutine.

3. Perform complex calculations on the arrays. In this case, it calculates `c(i) = a(i) + b(i) * sin(a(i)) + c(i) * cos(b(i))` for each element `i`.

4. Print the first 10 elements of the `c` array to display the results.

5. The `random_number` subroutine generates a random number between 0 and 1 and fills the input array with these random numbers.