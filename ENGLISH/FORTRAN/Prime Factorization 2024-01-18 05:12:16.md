```fortran
program prime_factors

implicit none

integer, parameter :: max_num = 10000000000

integer, dimension(max_num) :: is_prime

call sieve_of_eratosthenes(is_prime)

integer :: num
print *, "Enter an integer greater than 1: "
read *, num

if (num <= 1) then
    print *, "Invalid input. Number must be greater than 1."
else
    print *, "Prime factors of ", num, ":"

    integer :: i, j, count
    count = 0

    do i = 2, num

        if (is_prime(i)) then
            if (mod(num, i) == 0) then

                count = count + 1

                print *, i

                do
                    if (mod(num, i) == 0) then
                        num = num / i
                    else
                        exit
                    end if
                end do
            end if
        end if

    end do

    if (num > 1) then
        count = count + 1
        print *, num
    end if

    print *, "Total number of prime factors: ", count
end if

contains

subroutine sieve_of_eratosthenes(is_prime)

implicit none

integer, intent(inout) :: is_prime(max_num)

is_prime = 1

is_prime(1) = 0
is_prime(2) = 1

integer :: i, j

do i = 2, max_num

    if (is_prime(i)) then

        do j = i * 2, max_num, i

            is_prime(j) = 0

        end do

    end if

end do

end subroutine sieve_of_eratosthenes

end program prime_factors
```

Explanation:

This Fortran program finds all the prime factors of a given integer. It uses the Sieve of Eratosthenes algorithm to generate a table of prime numbers up to a certain limit. Then, it iterates through the prime numbers and checks if they divide the given integer. If they do, it prints the prime number and removes its multiples from the integer.

Here's a breakdown of the code:

1. **Module Declarations**:

   - `implicit none`: This specifies that all variables must be explicitly declared.
   - `integer, parameter :: max_num = 10000000000`: This sets the maximum number to be checked for primality.

2. **Array Declaration**:

   - `integer, dimension(max_num) :: is_prime`: This declares an array `is_prime` to store the primality status of numbers up to `max_num`.

3. **Subroutine Call**:

   - `call sieve_of_eratosthenes(is_prime)`: This calls the `sieve_of_eratosthenes` subroutine to populate the `is_prime` array.

4. **Input**:

   - `integer :: num`: This declares an integer variable `num` to store the input integer.
   - `print *, "Enter an integer greater than 1: "`: This prompts the user to enter an integer greater than 1.
   - `read *, num`: This reads the input integer from the user.

5. **Checking Validity**:

   - `if (num <= 1) then`: This checks if the input integer is less than or equal to 1. If it is, an error message is printed.

6. **Finding Prime Factors**:

   - `do i = 2, num`: This iterates through all integers from 2 to `num`, inclusive.
   - Inside the loop, there are nested `if` statements to check if `i` is prime and if it divides `num`. If both conditions are met, the following actions are taken:
     - `print *, i`: The prime factor `i` is printed.
     - `num = num / i`: The prime factor is removed from `num`.
   - This process continues until `num` is reduced to 1.

7. **Printing Total Prime Factors**:

   - `print *, "Total number of prime factors: ", count`: This prints the total number of prime factors found.

8. **Subroutine Definition**:

   - `subroutine sieve_of_eratosthenes(is_prime)`: This subroutine implements the Sieve of Eratosthenes algorithm to generate the `is_prime` array.

   - It iterates through the numbers from 2 to `max_num`, marks multiples of each prime number as non-prime, and updates the `is_prime` array accordingly.

9. **End of Program**:

   - `end program prime_factors`: This marks the end of the program.