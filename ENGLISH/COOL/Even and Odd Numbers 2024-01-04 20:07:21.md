```cool
class Main {
    static main() {
        Int i;
        i := 0;
        while (i < 10) {
            if (i % 2 == 0) {
                printInt(i);
            } else {
                printChar(' ');
                printInt(i);
            }
            i := i + 1;
        }
    }

    static printInt(Int i) {
        String s;
        s := intToStr(i);
        printString(s);
    }

    static printChar(Char c) {
        String s;
        s := charToStr(c);
        printString(s);
    }

    static printString(String s) {
        IO.printString(s);
    }

    static intToStr(Int i) {
        String s;
        if (i < 0) {
            s := "-";
            i := -i;
        } else {
            s := "";
        }
        while (i > 0) {
            Int digit;
            digit := i % 10;
            s := s + charToStr(digit + '0');
            i := i / 10;
        }
        return s;
    }

    static charToStr(Char c) {
        String s;
        s := "";
        s := s + c;
        return s;
    }
}
```

This code prints the numbers from 0 to 9, with the even numbers printed on the same line and the odd numbers printed on a separate line.

The `Main` class contains the `main` method, which is the entry point for the program. The `main` method creates an integer variable `i` and initializes it to 0. It then enters a `while` loop that continues until `i` is greater than or equal to 10.

Inside the `while` loop, the `main` method uses an `if` statement to check if `i` is even. If `i` is even, the `main` method calls the `printInt` method to print `i` on the same line. If `i` is odd, the `main` method calls the `printChar` method to print a space, followed by the `printInt` method to print `i` on a separate line.

The `main` method then increments `i` by 1 and continues to the next iteration of the `while` loop.

The `printInt` method takes an integer as an argument and converts it to a string. The `printChar` method takes a character as an argument and converts it to a string. The `printString` method takes a string as an argument and prints it to the console.

The `intToStr` method takes an integer as an argument and converts it to a string. The `charToStr` method takes a character as an argument and converts it to a string.