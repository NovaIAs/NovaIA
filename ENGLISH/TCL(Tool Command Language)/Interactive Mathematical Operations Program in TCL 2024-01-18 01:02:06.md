```tcl
proc long_factorial {n} {
    if {$n == 0} {
        return 1
    }
    return [expr {$n * [long_factorial [expr {$n - 1}]]}]
}

proc big_fibonacci {n} {
    if {$n < 2} {
        return $n
    }
    return [expr {[big_fibonacci [expr {$n - 1}]] + [big_fibonacci [expr {$n - 2}]]}]
}

proc is_prime {n} {
    if {$n <= 1} {
        return 0
    }
    for {set i 2} {$i * $i <= $n} {incr i} {
        if {[expr {$n % $i}] == 0} {
            return 0
        }
    }
    return 1
}

proc print_triangle {n} {
    for {set i 1} {$i <= $n} {incr i} {
        for {set j 0} {$j < $i} {incr j} {
            stdout write "* "
        }
        stdout write "\n"
    }
}

proc print_menu {options} {
    foreach item $options {
        stdout write "$item. $item\n"
    }
    stdout write "Please select an option: "
    flush stdout
    return [gets stdin]
}

proc main {} {
    while {1} {
        set choice [print_menu {factorial fibonacci prime triangle quit}]
        switch -glob $choice {
            "factorial*" {
                stdout write "Enter a number to compute its factorial: "
                flush stdout
                set n [gets stdin]
                stdout write "The factorial of $n is [long_factorial $n]\n"
            }
            "fibonacci*" {
                stdout write "Enter a number to compute its Fibonacci number: "
                flush stdout
                set n [gets stdin]
                stdout write "The Fibonacci number of $n is [big_fibonacci $n]\n"
            }
            "prime*" {
                stdout write "Enter a number to check if it is prime: "
                flush stdout
                set n [gets stdin]
                if {[is_prime $n]} {
                    stdout write "$n is a prime number.\n"
                } else {
                    stdout write "$n is not a prime number.\n"
                }
            }
            "triangle*" {
                stdout write "Enter the height of the triangle: "
                flush stdout
                set n [gets stdin]
                print_triangle $n
            }
            "quit" {
                exit
            }
            default {
                stdout write "Invalid choice. Please select a valid option.\n"
            }
        }
    }
}

main
```

This code is a TCL script that implements a simple interactive program that allows the user to choose from a menu of options to perform different mathematical calculations or operations. Here's a breakdown of the code:

1. **Factorial Calculation**:
   - The `long_factorial` procedure calculates the factorial of a given number `n`. It uses recursion to compute the factorial, with a base case for `n = 0`.

2. **Fibonacci Number Calculation**:
   - The `big_fibonacci` procedure calculates the Fibonacci number at a given position `n`. It also uses recursion, with base cases for `n < 2`.

3. **Primality Testing**:
   - The `is_prime` procedure checks whether a given number `n` is prime or not. It uses a loop to check for divisibility by numbers up to the square root of `n`.

4. **Triangle Printing**:
   - The `print_triangle` procedure prints a right triangle of asterisks with a height of `n`.

5. **Menu System**:
   - The `print_menu` procedure displays a menu of options for the user to choose from. It takes a list of options and prompts the user to select one.

6. **Main Program**:
   - The `main` procedure is the entry point of the program. It repeatedly displays the menu, gets the user's choice, and performs the corresponding action based on the user's input.

The user can select different options from the menu to calculate factorials, Fibonacci numbers, check for primality, print triangles, or quit the program.

This code demonstrates the use of procedures, functions, loops, and conditional statements in TCL to create a simple interactive program with various mathematical functionalities.