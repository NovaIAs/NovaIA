```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($num) = @_;
    my $result = 1;
    for (my $i = 1; $i <= $num; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to generate a list of prime numbers up to a given limit
sub prime_list {
    my ($limit) = @_;
    my @primes;
    for (my $i = 2; $i <= $limit; $i++) {
        my $is_prime = 1;
        for (my $j = 2; $j < $i; $j++) {
            if ($i % $j == 0) {
                $is_prime = 0;
                last;
            }
        }
        if ($is_prime) {
            push @primes, $i;
        }
    }
    return @primes;
}

# Define a subroutine to calculate the greatest common divisor of two numbers
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

# Get the input from the user
print "Enter a number to calculate the factorial: ";
my $number = <STDIN>;
chomp $number;

print "Enter a limit to generate a list of prime numbers: ";
my $limit = <STDIN>;
chomp $limit;

print "Enter two numbers to calculate the greatest common divisor: ";
my $num1 = <STDIN>;
chomp $num1;

my $num2 = <STDIN>;
chomp $num2;

# Calculate the factorial of the input number
my $factorial_result = factorial($number);

# Generate a list of prime numbers up to the input limit
my @prime_list_result = prime_list($limit);

# Calculate the greatest common divisor of the two input numbers
my $gcd_result = gcd($num1, $num2);

# Print the results
print "The factorial of $number is $factorial_result\n";
print "The list of prime numbers up to $limit is @prime_list_result\n";
print "The greatest common divisor of $num1 and $num2 is $gcd_result\n";
```

This code is a collection of three subroutines that perform different mathematical calculations:

1. **factorial()**: This subroutine calculates the factorial of a given number. The factorial of a number is the product of all positive integers up to that number. For example, the factorial of 5 is 120, which is calculated as 5 x 4 x 3 x 2 x 1.

2. **prime_list()**: This subroutine generates a list of prime numbers up to a given limit. A prime number is a natural number greater than 1 that has no positive divisors other than 1 and itself. For example, the prime numbers up to 10 are 2, 3, 5, and 7.

3. **gcd()**: This subroutine calculates the greatest common divisor (GCD) of two given numbers. The GCD of two numbers is the largest positive integer that divides both of them without leaving a remainder. For example, the GCD of 12 and 18 is 6.

The code first gets the input from the user for the three calculations. Then, it calls the appropriate subroutine to perform each calculation and prints the results.