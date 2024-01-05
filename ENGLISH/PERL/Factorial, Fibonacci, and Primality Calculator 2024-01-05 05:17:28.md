```perl
#!/usr/bin/perl

use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($n) = @_;

    # Base case: factorial of 0 is 1
    if ($n == 0) {
        return 1;
    }

    # Recursive case: factorial of n is n * factorial(n-1)
    else {
        return $n * factorial($n-1);
    }
}

# Define a subroutine to calculate the nth Fibonacci number
sub fibonacci {
    my ($n) = @_;

    # Base cases: fibonacci of 0 or 1 is 0 or 1, respectively
    if ($n == 0) {
        return 0;
    }
    elsif ($n == 1) {
        return 1;
    }

    # Recursive case: fibonacci of n is fibonacci(n-1) + fibonacci(n-2)
    else {
        return fibonacci($n-1) + fibonacci($n-2);
    }
}

# Define a subroutine to check if a number is prime
sub is_prime {
    my ($n) = @_;

    # Base cases: 0 and 1 are not prime
    if ($n == 0 || $n == 1) {
        return 0;
    }

    # Loop through all numbers from 2 to the square root of n
    for (my $i = 2; $i <= sqrt($n); $i++) {

        # If n is divisible by any number from 2 to its square root, it is not prime
        if ($n % $i == 0) {
            return 0;
        }
    }

    # If n is not divisible by any number from 2 to its square root, it is prime
    return 1;
}

# Get the user's input
print "Enter a positive integer: ";
my $n = <STDIN>;

# Check if the input is a valid positive integer
if ($n !~ /^[1-9][0-9]*$/) {
    print "Invalid input. Please enter a positive integer.\n";
    exit;
}

# Calculate the factorial, Fibonacci number, and primality of the input
my $factorial = factorial($n);
my $fibonacci = fibonacci($n);
my $is_prime = is_prime($n);

# Print the results
print "Factorial of $n: $factorial\n";
print "Fibonacci number of $n: $fibonacci\n";

if ($is_prime) {
    print "$n is a prime number.\n";
}
else {
    print "$n is not a prime number.\n";
}
```

This code is a Perl program that takes a positive integer as input and calculates its factorial, Fibonacci number, and primality. Here's how the code works:

1. The program starts by defining three subroutines: `factorial`, `fibonacci`, and `is_prime`.

2. The `factorial` subroutine calculates the factorial of a given number using recursion.

3. The `fibonacci` subroutine calculates the nth Fibonacci number using recursion.

4. The `is_prime` subroutine checks if a given number is prime by looping through all numbers from 2 to the square root of the number and checking if it is divisible by any of these numbers.

5. The program then prompts the user to enter a positive integer.

6. The program checks if the user's input is a valid positive integer.

7. The program calls the `factorial`, `fibonacci`, and `is_prime` subroutines to calculate the factorial, Fibonacci number, and primality of the input, respectively.

8. The program then prints the results to the console.

This code is a good example of how to use subroutines in Perl to modularize code and make it more reusable.