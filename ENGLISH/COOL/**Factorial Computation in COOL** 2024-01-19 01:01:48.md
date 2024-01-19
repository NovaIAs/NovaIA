```cool

/* Factorial is a mathematical function that returns the product of a non-negative integer and all the integers below it. It is usually denoted by the symbol ! (exclamation mark).
The factorial of a non-negative integer n is defined as the product of all the integers from 1 to n. For example, the factorial of 5 is 5! = 5 * 4 * 3 * 2 * 1 = 120. */

class Factorial {
    feature {
        /* This method computes the factorial of a non-negative integer n.
        It takes the integer n as a parameter and returns the factorial of n. */
        method factorial(n : Integer) : Integer {
            /* If n is less than or equal to 1, then the factorial of n is 1. */
            if n <= 1 then
                1
            else
                /* Otherwise, the factorial of n is n times the factorial of n-1. */
                n * factorial(n-1)
            fi
        };

        /* This method prints the factorial of a non-negative integer n.
        It takes the integer n as a parameter and prints the factorial of n to the console. */
        method printFactorial(n : Integer) : Void {
            /* Print the factorial of n to the console. */
            io.out_int(factorial(n));
            io.out_char('\n');
        };

        /* This method runs the program.
        It takes no parameters and returns nothing. */
        method main() : Void {
            /* Print the factorial of 10 to the console. */
            printFactorial(10);
        };
    };
};
```

**Explanation:**

1. **Class Definition:** The code defines a class named `Factorial` which encapsulates the factorial computation logic.

2. **Method `factorial`:**
   - Input: The method takes a non-negative integer `n` as its parameter.
   - Return: It returns the factorial of `n`.
   - Logic:
     - If `n` is less than or equal to 1, it returns 1 because the factorial of 1 is 1.
     - Otherwise, it recursively calls itself with `n-1` as the argument and multiplies the result by `n`. This is the recursive definition of factorial.

3. **Method `printFactorial`:**
   - Input: The method takes a non-negative integer `n` as its parameter.
   - Return: It returns nothing.
   - Logic: It calls the `factorial` method to compute the factorial of `n` and then prints it to the console using the `io.out_int` function.

4. **Method `main`:**
   - Input: The method takes no parameters.
   - Return: It returns nothing.
   - Logic: It calls the `printFactorial` method with the value 10 as the argument to print the factorial of 10 to the console.

5. **Running the Program:** To run the program, you would need a COOL compiler and runtime environment. Once you have those set up, you can compile and run the program using the appropriate commands.