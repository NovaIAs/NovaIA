Example 1: Staircase Pattern
```
      program Staircase
      implicit none
      integer :: n, i, j

      print*, "Enter the number of rows: "
      read*, n

      do i = 1, n
         do j = 1, i
            print*, '*'
         end do
         print*, ""
      end do

      end program Staircase
```

Explanation:
This program creates a staircase pattern of asterisks. The user is prompted to enter the number of rows, and the program uses nested loops to print the appropriate number of asterisks in each row.

Example 2: Fibonacci Sequence
```
      program Fibonacci
      implicit none
      integer :: n, i, a, b, c

      print*, "Enter the number of terms: "
      read*, n

      a = 0
      b = 1
      do i = 1, n
         c = a + b
         print*, c
         a = b
         b = c
      end do

      end program Fibonacci
```
Explanation:
This program generates the Fibonacci sequence up to the specified number of terms. The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding numbers. The program starts with the first two terms (0 and 1) and uses a loop to calculate and print the subsequent terms.

Example 3: Prime Number Checker
```
program PrimeChecker
implicit none
integer :: n, i, flag

print*, 'Enter a number:'
read*, n

if (n <= 1) then
    print*, 'Not a prime number'
else
    flag = 1
    i = 2
    do while (i <= n / 2 .and. flag == 1)
        if (mod(n, i) == 0) then
            flag = 0
        end if
        i = i + 1
    end do

    if (flag == 1) then
        print*, 'Prime number'
    else
        print*, 'Not a prime number'
    end if
end if

end program PrimeChecker
```

Explanation:
This program checks whether a given number is prime or not. It uses a loop to check if the number is divisible by any integer from 2 to half the number. If the number is divisible by any of these integers, it is not prime, otherwise, it is prime.