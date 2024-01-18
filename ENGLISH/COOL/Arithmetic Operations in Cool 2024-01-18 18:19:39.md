```cool
class Main {
    static main() { // program entry point
        var a : Int <- 10; // assign 10 to a
        var b : Int <- 20; // assign 20 to b
        var c : Int <- a + b; // assign the sum of a and b to c
        var d : Int <- a - b; // assign the difference of a and b to d
        var e : Int <- a * b; // assign the product of a and b to e
        var f : Int <- a / b; // assign the quotient of a and b to f
        var g : Int <- a % b; // assign the remainder of a and b to g

        // print the values of a, b, c, d, e, f, and g
        IO.out_int(a);
        IO.out_char('\n');
        IO.out_int(b);
        IO.out_char('\n');
        IO.out_int(c);
        IO.out_char('\n');
        IO.out_int(d);
        IO.out_char('\n');
        IO.out_int(e);
        IO.out_char('\n');
        IO.out_int(f);
        IO.out_char('\n');
        IO.out_int(g);
        IO.out_char('\n');
    }
}
```

This program declares several variables of type `Int` and assigns them values. It then performs various arithmetic operations on these variables and prints the results to the console. The program uses the `IO` class to perform input and output operations.

Here is a breakdown of the program:

* Class `Main`: This is the main class of the program. It contains the `main` method, which is the program entry point.
* `static main()`: This is the `main` method of the program. It is declared as `static` because it does not belong to any object instance.
* `var a : Int <- 10;`: This line declares a variable `a` of type `Int` and assigns it the value 10.
* `var b : Int <- 20;`: This line declares a variable `b` of type `Int` and assigns it the value 20.
* `var c : Int <- a + b;`: This line declares a variable `c` of type `Int` and assigns it the sum of `a` and `b`.
* `var d : Int <- a - b;`: This line declares a variable `d` of type `Int` and assigns it the difference of `a` and `b`.
* `var e : Int <- a * b;`: This line declares a variable `e` of type `Int` and assigns it the product of `a` and `b`.
* `var f : Int <- a / b;`: This line declares a variable `f` of type `Int` and assigns it the quotient of `a` and `b`.
* `var g : Int <- a % b;`: This line declares a variable `g` of type `Int` and assigns it the remainder of `a` and `b`.
* `IO.out_int(a);`: This line calls the `out_int` method of the `IO` class to print the value of `a` to the console.
* `IO.out_char('\n');`: This line calls the `out_char` method of the `IO` class to print a newline character to the console.

The program continues to print the values of `b`, `c`, `d`, `e`, `f`, and `g` to the console, each followed by a newline character.