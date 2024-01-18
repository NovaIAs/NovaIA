```fortran
program prime_factorization
implicit none

integer, parameter :: max_number = 1000000  ! Maximum number to factorize

integer :: number, factor, exponent
logical :: is_prime

print *, "Enter a positive integer to factorize (<= ", max_number, "): "
read *, number

if (number <= 1) then
  print *, "Invalid input: number must be a positive integer greater than 1."
else if (number > max_number) then
  print *, "Invalid input: number must be less than or equal to ", max_number
else
  print *, "Prime factorization of ", number, ":"

  factor = 2
  while (number > 1) do
    is_prime = .true.
    while (mod(number, factor) /= 0) do
      factor = factor + 1
      is_prime = .false.
    end while

    if (is_prime) then
      exponent = 0
      do while (mod(number, factor) == 0)
        exponent = exponent + 1
        number = number / factor
      end do
      print *, factor, "^", exponent
    end if
  end while
end if

end program prime_factorization
```

Explanation:

1. **Constants and Parameters:**

   - `max_number` is a constant parameter that sets the maximum number that can be factorized. This helps prevent errors if the user enters a very large number.

2. **Input Validation:**

   - The program checks if the user's input is valid, i.e., a positive integer greater than 1 and less than or equal to `max_number`.

3. **Prime Factorization Loop:**

   - The program uses a `while` loop to repeatedly find and print the prime factors of the input number.

4. **Finding Prime Factors:**

   - It starts with `factor = 2` because all even numbers are divisible by 2.

   - The `while` loop finds the smallest prime factor by incrementing `factor` until it evenly divides `number`.

   - If `factor` evenly divides `number`, it is a prime factor, and the program calculates its exponent using another `while` loop.

   - The exponent is the number of times `factor` can be repeatedly divided into `number`.

5. **Printing the Result:**

   - The program prints the prime factor and its exponent in the format `factor^exponent`.

   - It continues this process until all prime factors of `number` are found.