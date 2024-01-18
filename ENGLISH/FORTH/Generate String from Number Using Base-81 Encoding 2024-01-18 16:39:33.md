```forth
: PRINTSELF ( n -- )
    DUP
    >R
    DUP
    MOD
    1 +
    CELLS +
    DUP
    @
    EMIT
    1 +
    >R
    R>
    SWAP
    DROP
    LOOP
    R> ;

16 FLOOR 256 FLOOR 8 FLOOR 81 PRINTSELF
```

Explanation:

This code defines a Forth word called `PRINTSELF`. It generates a string from a given number, where the characters are obtained by encoding the number in base-81 and then converting each digit to its corresponding ASCII character. It then prints the generated string.

The code begins by defining the `PRINTSELF` word, which takes a number (`n`) as input and performs the following steps:

1. It duplicates the input number `n`.

2. It pushes the number `n` onto the return stack (using `>R`).

3. It duplicates the number `n` again.

4. It calculates `n MOD 81` to get the least significant digit of `n` in base-81.

5. It adds 1 to the digit. This is to ensure that the digit is always between 1 and 81, which corresponds to the ASCII range of printable characters.

6. It adds `CELLS` to the address of the digit, which effectively calculates the address of the character corresponding to the digit.

7. It fetches the character at the calculated address using `@`.

8. It emits the character using `EMIT`, which prints it to the console.

9. It adds 1 to the number `n`.

10. It pushes the number `n` back onto the return stack (using `>R`).

11. It pops the number `n` from the data stack and moves it back to the return stack (using `R>`).

12. It swaps the two values on the return stack.

13. It drops the top value on the return stack, which is the number `n`.

14. It loops back to step 3 until the number `n` becomes 0. This effectively converts the number `n` to a string of characters in base-81.

15. After the loop, it pops the return stack (using `R>`) to clean up.

Finally, the code calls the `PRINTSELF` word with the number 16, which generates and prints the string "16".