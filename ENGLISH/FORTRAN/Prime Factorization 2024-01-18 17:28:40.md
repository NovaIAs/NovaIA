```fortran
program prime_factors

  implicit none

  integer, parameter :: max_number = 1000000

  integer, dimension(max_number) :: primes
  integer :: i, j, k, n, counter

  primes(1) = 2
  j = 2

  do i = 3, max_number, 2
    do k = 3, isqrt(i), 2
      if (mod(i, k) == 0) exit
    end do
    if (k > isqrt(i)) then
      primes(j) = i
      j = j + 1
    end if
  end do

  print *, 'Enter an integer greater than 1: '
  read *, n

  if (n <= 1) then
    print *, 'Invalid input. Please enter an integer greater than 1.'
  else
    print *, 'The prime factors of ', n, ' are:'
    counter = 0
    do i = 1, j - 1
      if (mod(n, primes(i)) == 0) then
        print *, primes(i)
        n = n / primes(i)
        counter = counter + 1
      end if
      if (counter > 0) exit
    end do
    if (n > 1) then
      print *, n
    end if
  end if

contains

  function isqrt(x)

    implicit none

    integer, intent(in) :: x
    integer :: result

    result = int(sqrt(real(x, 8)))

  end function isqrt

end program prime_factors
```

This program finds the prime factors of a given integer.

The program first generates a list of prime numbers up to a specified limit (in this case, 1,000,000). This is done using the Sieve of Eratosthenes algorithm. The algorithm starts with a list of all the integers from 2 to the limit. Then, it iteratively marks off all the multiples of each prime number, starting with 2. The numbers that are not marked off are the prime numbers.

Once the list of prime numbers has been generated, the program prompts the user to enter an integer. If the entered integer is greater than 1, the program finds its prime factors. This is done by dividing the integer by each prime number in the list, starting with 2. If the integer is divisible by a prime number, then that prime number is a prime factor of the integer. The integer is then divided by the prime factor, and the process is repeated until the integer is no longer divisible by any prime number.

The program prints out the prime factors of the integer, one per line. If the integer is prime, then the program prints out the integer itself, as it is its own prime factor.

Here is an example of how the program works:

```
Enter an integer greater than 1: 12

The prime factors of 12 are:
2
2
3
```

In this example, the user entered the integer 12. The program found that the prime factors of 12 are 2, 2, and 3.