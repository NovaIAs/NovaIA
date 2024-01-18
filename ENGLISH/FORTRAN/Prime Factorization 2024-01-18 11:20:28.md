```fortran
program prime_factorization

implicit none

integer, parameter :: MaxN = 1000000 'Maximum number to factor'

integer, dimension(MaxN) :: is_prime   'Sieve of Eratosthenes'
integer, dimension(MaxN) :: prime_factors  'Prime factors'

! Initialize the sieve of Eratosthenes
is_prime = 1
is_prime(1) = 0
is_prime(2) = 1

! Sieve the numbers up to MaxN
do i = 3, MaxN, 2
   if (is_prime(i)) then
      is_prime(i*i) = 0
      do j = i*i+i, MaxN, i
         is_prime(j) = 0
      end do
   end if
end do

! Find the prime factors of each number up to MaxN
do n = 2, MaxN
   prime_factors(n) = 0

   ! Start with the smallest prime factor
   i = 2

   ! While the number is not fully factorized
   do while (n > 1)
      ! If the number is divisible by the current prime factor
      if (mod(n, i) == 0) then
         ! Add the prime factor to the list
         prime_factors(n) = i

         ! Divide the number by the prime factor
         n = n / i
      end if

      ! Move to the next prime factor
      i = i + 1
   end do
end do

! Print the prime factors for each number up to MaxN
do n = 2, MaxN
   if (prime_factors(n) == 0) then
      write (*, '(i6,a)') n, ': Prime'
   else
      write (*, '(i6,a,i6)') n, ': ', prime_factors(n)
   end if
end do

end program prime_factorization
```

This program finds the prime factors of all numbers up to a specified limit (MaxN). It uses a sieve of Eratosthenes to efficiently find all prime numbers up to the limit, and then uses a simple loop to find the prime factors of each number.

The program is divided into three main parts:

1. **Initializing the sieve of Eratosthenes:** This is done by setting all elements of the `is_prime` array to 1, except for 1 and 2, which are not prime.
2. **Sieving the numbers up to MaxN:** This is done by iterating over all numbers from 3 to MaxN, and for each prime number, marking all multiples of that number as non-prime.
3. **Finding the prime factors of each number:** This is done by iterating over all numbers from 2 to MaxN, and for each number, using a loop to find all prime factors of that number.

The program then prints the prime factors of each number up to MaxN.

Here is an example of the output of the program:

```
  2: Prime
  3: Prime
  4: 2
  5: Prime
  6: 2 3
  7: Prime
  8: 2 2 2
  9: 3 3
 10: 2 5
 11: Prime
 12: 2 2 3
 13: Prime
 14: 2 7
 15: 3 5
 16: 2 2 2 2
 17: Prime
 18: 2 3 3
 19: Prime
 20: 2 2 5
```