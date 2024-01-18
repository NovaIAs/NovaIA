```cool
class Main {
    static Main main() {
        IO.out_int(Fibonacci.fib(10));
        IO.out_char('\n');
        return 0;
    }
}

class Fibonacci {
    static int fib(int n) {
        if (n <= 1) {
            return n;
        } else {
            return fib(n - 1) + fib(n - 2);
        }
    }
}

class IO {
    static void out_int(int n) {
        String s = String.new(n);
        out_string(s);
    }

    static void out_char(char c) {
        System.out.print(c);
    }

    static void out_string(String s) {
        System.out.print(s);
    }
}

class String {
    String str;

    static String new(int n) {
        String s = new String();
        s.str = "" + n;
        return s;
    }
}
```

This program calculates the 10th Fibonacci number using a recursive function. The code consists of three classes: Main, Fibonacci, and IO. The Main class contains the main function, which calls the fib function in the Fibonacci class to calculate the 10th Fibonacci number. The Fibonacci class contains the fib function, which recursively calculates the nth Fibonacci number. The IO class contains functions for printing integers, characters, and strings to the console.

Here is a step-by-step explanation of the code:

1. The Main class defines the main function, which is the entry point of the program.

2. The main function calls the fib function in the Fibonacci class with the argument 10 to calculate the 10th Fibonacci number.

3. The fib function in the Fibonacci class is a recursive function that calculates the nth Fibonacci number. If n is 0 or 1, the function returns n. Otherwise, the function calls itself with the arguments n - 1 and n - 2, and returns the sum of the results of these two calls.

4. The IO class defines functions for printing integers, characters, and strings to the console. The out_int function converts an integer to a string and then calls the out_string function to print the string to the console. The out_char function prints a character to the console. The out_string function prints a string to the console.

5. The String class defines a string type. The new function in the String class creates a new string object and initializes it with the value of the integer argument.

6. The main function calls the out_int function to print the 10th Fibonacci number to the console, followed by a newline character.

When you run this program, it will calculate the 10th Fibonacci number (which is 55) and print it to the console.