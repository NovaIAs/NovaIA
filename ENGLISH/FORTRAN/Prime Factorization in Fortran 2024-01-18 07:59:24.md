```fortran
program prime_factorization
  implicit none
  integer :: n, i, j, count, prime
  print *, "Enter an integer: "
  read *, n
  print *, "Prime factors of", n, ":"
  i = 2
  do while (n > 1)
    if (mod(n, i) == 0) then
      prime = i
      count = 0
      do while (mod(n, i) == 0)
        count = count + 1
        n = n / i
      end do
      print *, prime, "^", count
    end if
    i = i + 1
  end do
end program prime_factorization
```

Explanation:

1. `program prime_factorization`: This line declares the start of a Fortran program named `prime_factorization`.

2. `implicit none`: This line tells the compiler that no implicit typing will be used. This means that every variable must be explicitly declared with its type.

3. `integer :: n, i, j, count, prime`: This line declares five integer variables: `n`, `i`, `j`, `count`, and `prime`. These variables will be used to store various values during the program.

4. `print *, "Enter an integer: "` and `read *, n`: These lines prompt the user to enter an integer and read the input into the variable `n`.

5. `print *, "Prime factors of", n, ":"`: This line prints a message to the console indicating that the program will now display the prime factors of the entered integer.

6. `i = 2`: This line initializes the variable `i` to 2. This variable will be used to iterate through potential prime factors.

7. `do while (n > 1)`: This line starts a `do while` loop that will continue as long as `n` is greater than 1. This loop will be used to find and print the prime factors of `n`.

8. `if (mod(n, i) == 0)`: Inside the `do while` loop, this `if` statement checks if `i` is a prime factor of `n`. It does this by using the `mod()` function to calculate the remainder when `n` is divided by `i`. If the remainder is 0, then `i` is a prime factor of `n`.

9. `prime = i`: If `i` is a prime factor of `n`, this line assigns the value of `i` to the variable `prime` to store the prime factor.

10. `count = 0`: This line initializes the variable `count` to 0. This variable will be used to count how many times `i` divides `n`.

11. `do while (mod(n, i) == 0)`: This line starts a nested `do while` loop that will continue as long as `i` divides `n` evenly. This loop is used to count the number of times `i` appears as a prime factor of `n`.

12. `count = count + 1`: Inside the nested `do while` loop, this line increments the `count` variable by 1 for each time `i` divides `n` evenly.

13. `n = n / i`: This line divides `n` by `i` to remove the prime factor from `n`. This process is repeated until `i` no longer divides `n` evenly.

14. `print *, prime, "^", count`: After the nested `do while` loop completes, this line prints the prime factor along with the exponent (the value of `count`).

15. `i = i + 1`: This line increments the value of `i` by 1 to check the next potential prime factor.

16. `end do`: This line ends the `do while` loop from step 7.

17. `end program prime_factorization`: This line marks the end of the `prime_factorization` program.