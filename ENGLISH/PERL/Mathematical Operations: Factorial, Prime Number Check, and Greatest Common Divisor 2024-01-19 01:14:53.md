```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($number) = @_;
    if ($number == 1) {
        return 1;
    } else {
        return $number * factorial($number - 1);
    }
}

# Get the input from the user
print "Enter a positive integer: ";
my $input = <STDIN>;
chomp($input);

# Check if the input is a valid positive integer
if ($input !~ /^[1-9]\d*$/) {
    die "Error: $input is not a valid positive integer.";
}

# Calculate the factorial of the input and store it in a variable
my $factorial = factorial($input);

# Print the result to the console
print "The factorial of $input is $factorial.\n";

# Define a subroutine to check if a number is prime
sub is_prime {
    my ($number) = @_;
    if ($number == 1) {
        return 0;
    } elsif ($number == 2) {
        return 1;
    } elsif ($number % 2 == 0) {
        return 0;
    } else {
        for (my $i = 3; $i * $i <= $number; $i += 2) {
            if ($number % $i == 0) {
                return 0;
            }
        }
        return 1;
    }
}

# Get the input from the user
print "Enter a positive integer: ";
my $input = <STDIN>;
chomp($input);

# Check if the input is a valid positive integer
if ($input !~ /^[1-9]\d*$/) {
    die "Error: $input is not a valid positive integer.";
}

# Check if the input is prime and store the result in a variable
my $is_prime = is_prime($input);

# Print the result to the console
if ($is_prime) {
    print "$input is a prime number.\n";
} else {
    print "$input is not a prime number.\n";
}

# Define a subroutine to find the greatest common divisor of two numbers
sub gcd {
    my ($a, $b) = @_;
    if ($b == 0) {
        return $a;
    } else {
        return gcd($b, $a % $b);
    }
}

# Get the input from the user
print "Enter two positive integers: ";
my $input1 = <STDIN>;
chomp($input1);
my $input2 = <STDIN>;
chomp($input2);

# Check if the input is a valid positive integer
if ($input1 !~ /^[1-9]\d*$/ || $input2 !~ /^[1-9]\d*$/) {
    die "Error: $input1 or $input2 is not a valid positive integer.";
}

# Calculate the greatest common divisor of the two inputs and store it in a variable
my $gcd = gcd($input1, $input2);

# Print the result to the console
print "The greatest common divisor of $input1 and $input2 is $gcd.\n";
```

This code is a collection of three different subroutines, each of which performs a different mathematical operation. The first subroutine calculates the factorial of a number, the second subroutine checks if a number is prime, and the third subroutine finds the greatest common divisor of two numbers. The code prompts the user to enter input values for each subroutine, checks if the input is valid, and then prints the result of the operation to the console.

Here is a breakdown of each subroutine:

* **factorial:** This subroutine takes a single argument, which is the number to calculate the factorial of. The factorial of a number is the product of all the positive integers up to and including that number. For example, the factorial of 5 is 120, because 5! = 5 * 4 * 3 * 2 * 1. The subroutine uses recursion to calculate the factorial. It checks if the input is equal to 1, in which case it returns 1. Otherwise, it multiplies the input by the factorial of the input minus 1.


* **is_prime:** This subroutine takes a single argument, which is the number