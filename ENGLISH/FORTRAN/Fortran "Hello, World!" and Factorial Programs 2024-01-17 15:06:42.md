```fortran
PROGRAM HELLO_WORLD
  WRITE (*, *) 'HELLO, WORLD!'
END PROGRAM HELLO_WORLD
```

Explanation:

* The first line `PROGRAM HELLO_WORLD` is the program header. It declares the name of the program as `HELLO_WORLD`.
* The second line `WRITE (*, *) 'HELLO, WORLD!'` is the write statement. It prints the string `HELLO, WORLD!` to the standard output (usually the console).
* The third line `END PROGRAM HELLO_WORLD` is the program terminator. It indicates the end of the program.

The above code is a simple "Hello, World!" program in Fortran. It is a common program often used to introduce a new programming language.

Here is a more complex Fortran code that calculates the factorial of a number:

```fortran
PROGRAM FACTORIAL
  INTEGER :: N, FACT
  WRITE (*, *) 'ENTER A NUMBER:'
  READ (*, *) N
  FACT = 1
  DO I = 2, N
    FACT = FACT * I
  END DO
  WRITE (*, *) 'FACTORIAL OF', N, 'IS', FACT
END PROGRAM FACTORIAL
```

Explanation:

* The first line `PROGRAM FACTORIAL` is the program header. It declares the name of the program as `FACTORIAL`.
* The second line `INTEGER :: N, FACT` declares two integer variables `N` and `FACT`. `N` will store the input number and `FACT` will store the factorial of the number.
* The third line `WRITE (*, *) 'ENTER A NUMBER:'` prompts the user to enter a number.
* The fourth line `READ (*, *) N` reads the input number from the console and stores it in the variable `N`.
* The fifth line `FACT = 1` initializes the variable `FACT` to 1. This is done because the factorial of 0 is 1.
* The sixth line `DO I = 2, N` starts a `DO` loop that iterates from 2 to `N`.
* The seventh line `FACT = FACT * I` calculates the factorial by multiplying `FACT` by the current value of `I`.
* The eighth line `END DO` ends the `DO` loop.
* The ninth line `WRITE (*, *) 'FACTORIAL OF', N, 'IS', FACT` prints the factorial of `N` to the console.
* The tenth line `END PROGRAM FACTORIAL` is the program terminator. It indicates the end of the program.

The above code is a more complex Fortran program that calculates the factorial of a number. It uses a `DO` loop to iterate from 2 to the input number and calculate the factorial by multiplying the current value of the factorial by the current value of the loop variable.