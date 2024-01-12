```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($number) = @_;
    my $result = 1;
    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to calculate the Fibonacci sequence
sub fibonacci {
    my ($number) = @_;
    my @sequence = (0, 1);
    while ($sequence[-1] < $number) {
        my $next_number = $sequence[-2] + $sequence[-1];
        push @sequence, $next_number;
    }
    return @sequence;
}

# Define a subroutine to check if a number is prime
sub is_prime {
    my ($number) = @_;
    if ($number <= 1) {
        return 0;
    }
    for (my $i = 2; $i <= sqrt($number); $i++) {
        if ($number % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# Define a subroutine to find the greatest common divisor of two numbers
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

# Define a subroutine to find the least common multiple of two numbers
sub lcm {
    my ($a, $b) = @_;
    return ($a * $b) / gcd($a, $b);
}

# Print the results of the subroutines
print "Factorial of 5: ", factorial(5), "\n";
print "Fibonacci sequence up to 100: ", join(", ", fibonacci(100)), "\n";
print "Is 7 prime? ", is_prime(7), "\n";
print "Greatest common divisor of 12 and 18: ", gcd(12, 18), "\n";
print "Least common multiple of 9 and 15: ", lcm(9, 15), "\n";
```

Explanation:

This Perl code defines five subroutines:

1. `factorial`: Calculates the factorial of a given number.

2. `fibonacci`: Generates the Fibonacci sequence up to a given number.

3. `is_prime`: Checks if a given number is prime.

4. `gcd`: Finds the greatest common divisor of two given numbers.

5. `lcm`: Finds the least common multiple of two given numbers.

The code then uses these subroutines to perform various calculations and prints the results. For example, it calculates the factorial of 5, generates the Fibonacci sequence up to 100, checks if 7 is prime, finds the greatest common divisor of 12 and 18, and finds the least common multiple of 9 and 15.

This code demonstrates the use of subroutines in Perl, which are similar to functions in other programming languages. Subroutines allow you to break down a program into smaller, more manageable pieces, making it easier to read, understand, and maintain.