```cool

class Main {
    static main() {
        let i: Int <- 0;
        while (i < 10) {
            let j: Int <- 0;
            while (j < 10) {
                let k: Int <- 0;
                while (k < 10) {
                    IO.print_int(i);
                    IO.print_int(j);
                    IO.print_int(k);
                    IO.print_newline();
                    k <- k + 1;
                };
                j <- j + 1;
            };
            i <- i + 1;
        };
    };
};

```

Explanation:

This code is a nested loop that prints all the combinations of three numbers from 0 to 9. The outer loop (controlled by the variable `i`) iterates over the first number, the middle loop (controlled by the variable `j`) iterates over the second number, and the inner loop (controlled by the variable `k`) iterates over the third number.

For each combination of numbers, the code prints the numbers to the console using the `IO.print_int()` function, followed by a newline character using the `IO.print_newline()` function.

The code uses the `while` statement to control the loops. The `while` statement takes a Boolean expression as its argument, and the loop will continue to execute as long as the expression evaluates to `true`.

The code also uses the `let` statement to declare and initialize variables. The `let` statement takes a variable name and an expression as its arguments, and the variable is assigned the value of the expression.

The code uses the `<-` operator to assign values to variables. The `<-` operator is used to assign the value of an expression to a variable, and it also returns the value of the expression.

The code uses the `+` operator to add two numbers together. The `+` operator takes two numbers as its arguments, and it returns the sum of the two numbers.

The code uses the `<` operator to compare two numbers. The `<` operator takes two numbers as its arguments, and it returns `true` if the first number is less than the second number, and `false` otherwise.