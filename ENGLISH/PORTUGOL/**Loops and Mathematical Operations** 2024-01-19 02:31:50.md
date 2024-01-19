```portugal
LOOP SUM-NUMBERS
    SUM:=0
    LOOP FOR I:=1 TO 100 DO
        SUM:=SUM+I
    END-LOOP-FOR
    PRINT("The sum of the numbers from 1 to 100 is ", SUM)
END-LOOP-SUM-NUMBERS

LOOP FACTORIAL
    LOOP FOR I:=1 TO 10 DO
        NUMBER:=1
        LOOP FOR J:=1 TO I DO
            NUMBER:=NUMBER*J
        END-LOOP-FOR
        PRINT("The factorial of ", I, " is ", NUMBER)
    END-LOOP-FOR
END-LOOP-FACTORIAL

LOOP PERFECT-NUMBERS
    LOOP FOR I:=1 TO 1000 DO
        SUM:=0
        LOOP FOR J:=1 TO I DO
            IF I MOD J=0 THEN
                SUM:=SUM+J
            END-IF
        END-LOOP-FOR
        IF SUM=I THEN
            PRINT(I, " is a perfect number")
        END-IF
    END-LOOP-FOR
END-LOOP-PERFECT-NUMBERS

LOOP PRIME-NUMBERS
    LOOP FOR I:=2 TO 100 DO
        PRIME:=1
        LOOP FOR J:=2 TO I-1 DO
            IF I MOD J=0 THEN
                PRIME:=0
                JUMP OUT-OF-LOOP-PRIME
            END-IF
        END-LOOP-FOR
        IF PRIME=1 THEN
            PRINT(I, " is a prime number")
        END-IF
    OUT-OF-LOOP-PRIME:
    END-LOOP-FOR
END-LOOP-PRIME-NUMBERS
```

This code is a collection of loops that perform various mathematical operations. Let's break down each loop:

1. **LOOP SUM-NUMBERS:** This loop calculates the sum of numbers from 1 to 100. It uses a nested loop to iterate through all the numbers and adds them to the `SUM` variable.

2. **LOOP FACTORIAL:** This loop calculates the factorial of numbers from 1 to 10. It initializes a variable `NUMBER` to 1 and then iterates through each number from 1 to the current number, multiplying `NUMBER` by that number. Finally, it prints the factorial of the current number.

3. **LOOP PERFECT-NUMBERS:** This loop finds perfect numbers between 1 and 1000. A perfect number is a positive integer that is equal to the sum of its proper divisors (excluding the number itself). The loop iterates through each number from 1 to 1000, calculates the sum of its proper divisors, and checks if the sum is equal to the number itself. If so, it prints the number as a perfect number.

4. **LOOP PRIME-NUMBERS:** This loop finds prime numbers between 2 and 100. A prime number is a positive integer greater than 1 that has no positive divisors other than 1 and itself. The loop iterates through each number from 2 to 100 and checks if it is divisible by any number from 2 to the number itself. If it finds a divisor, it marks the number as non-prime and moves to the next number. If it reaches the end of the loop without finding a divisor, it prints the number as a prime number.