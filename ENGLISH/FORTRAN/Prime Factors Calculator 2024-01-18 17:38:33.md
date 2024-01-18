```fortran
program prime_factors
implicit none

integer :: n, i, j, prime, count
logical :: is_prime

print *, "Enter a positive integer: "
read *, n

if (n <= 1) then
  print *, "1 has no prime factors."
  stop
end if

print *, "The prime factors of", n, "are:"

i = 2

do while (i <= n)
  j = 2
  is_prime = .true.  ! assume i is prime

  do while (j < i)
    if (mod(i, j) == 0) then
      is_prime = .false.  ! i is not prime
      exit
    end if
    j = j + 1
  end do

  if (is_prime) then
    prime = i
    count = 0

    do while (mod(n, prime) == 0)
      count = count + 1
      n = n / prime
    end do

    if (count > 0) then
      print *, prime, "^", count
    end if
  end if

  i = i + 1
end do

end program prime_factors
```

Explanation:

This Fortran program calculates the prime factors of a given positive integer `n`. It uses a simple algorithm to check if a number is prime and then divides `n` by that prime number as long as it is divisible.

Here's a breakdown of the code:

1. **Input**: The program starts by prompting the user to enter a positive integer.

2. **Checking for Invalid Input**: It checks if the entered number is less than or equal to 1. If it is, the program prints a message indicating that 1 has no prime factors and terminates.

3. **Calculating Prime Factors**:

   - It initializes a variable `i` to 2, which is the first prime number.

   - It enters a `do while` loop that continues as long as `i` is less than or equal to `n`.

   - Inside the loop, it checks if `i` is a prime number using a nested `do while` loop.

   - If `i` is prime, it calculates the highest power of `i` that divides `n` and prints the prime factor and its power.

   - It then divides `n` by the prime factor to get the next number to be checked for prime factors.

4. **Output**: The program prints the prime factors of `n` in the format "`prime^power`", where `prime` is the prime factor and `power` is the highest power of that prime factor that divides `n`.

This program provides a detailed explanation of the process of finding prime factors, making it easier to understand the algorithm and its implementation.