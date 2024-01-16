```cool
class Main inherits IO {
    main() : Object {
        sum := 0;
        for i in 1 to 100 loop
            sum := sum + i;
        end loop;
        out := sum;
        return out;
    };
};
```

This code is a simple program that calculates the sum of the first 100 natural numbers. It uses a loop to iterate over the numbers from 1 to 100, and adds each number to the sum. The final value of the sum is printed to the console.

Here is a breakdown of the code:

* The `Main` class inherits from the `IO` class, which provides methods for input and output.
* The `main` method is the entry point of the program. It returns an `Object`, which is the type of the value that is printed to the console.
* The `sum` variable is used to store the sum of the numbers.
* The `for` loop iterates over the numbers from 1 to 100. The `loop` keyword is used to start the loop, and the `end loop` keyword is used to end the loop.
* The `+` operator is used to add the current number to the sum.
* The `out` variable is used to store the final value of the sum.
* The `return` statement is used to return the value of `out` to the caller.

This code is a good example of how to use loops and variables in COOL. It is also a good example of how to use the `IO` class to print values to the console.