```cool
--- Program to calculate the greatest common divisor (gcd) of two numbers. ---

class GCD {
    static gcd(n1: Int, n2: Int): Int {
        if n2 = 0 then
            return n1
        else
            return gcd(n2, n1 % n2)
        fi
    }

    static main: () -> Int {
        n1 := readInt()
        n2 := readInt()
        return gcd(n1, n2)
    }
}
```

Explanation:

1. `class GCD {`: This line starts the definition of a class named `GCD`. A class is a blueprint for creating objects, and it encapsulates data and behavior.

2. `static gcd(n1: Int, n2: Int): Int {`: This line defines a static method named `gcd` within the `GCD` class. It takes two integer parameters, `n1` and `n2`, and returns an integer result.

3. `if n2 = 0 then`: This line checks if the value of `n2` is equal to 0. If so, it means that `n1` is the greatest common divisor, and the method returns `n1` using the `return` statement.

4. `else`: If `n2` is not equal to 0, the `else` block is executed.

5. `return gcd(n2, n1 % n2)`: This line recursively calls the `gcd` method with two new parameters: `n2` and the remainder of `n1` divided by `n2` (`n1 % n2`). This recursive call continues until `n2` becomes 0, at which point the greatest common divisor is found.

6. `static main: () -> Int {`: This line defines the `main` method, which is the entry point for the program. It has an empty parameter list and returns an integer.

7. `n1 := readInt()`: Reads an integer from the standard input and assigns it to the variable `n1`.

8. `n2 := readInt()`: Reads another integer from the standard input and assigns it to the variable `n2`.

9. `return gcd(n1, n2)`: Calls the `gcd` method with `n1` and `n2` as arguments and returns the result.

10. `}`: This line ends the definition of the `GCD` class.