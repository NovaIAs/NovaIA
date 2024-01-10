```perl
use strict;
use warnings;

# Define the main module
package main;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($n) = @_;
    return 1 if $n == 0;
    return $n * factorial($n - 1);
}

# Define a subroutine to generate a random number between two numbers
sub random_number {
    my ($min, $max) = @_;
    return int(rand($max - $min + 1)) + $min;
}

# Define a subroutine to print a greeting message
sub greet {
    my ($name) = @_;
    print "Hello, $name!\n";
}

# Define a subroutine to calculate the area of a circle
sub circle_area {
    my ($radius) = @_;
    return pi() * $radius ** 2;
}

# Define a subroutine to calculate the volume of a sphere
sub sphere_volume {
    my ($radius) = @_;
    return (4 / 3) * pi() * $radius ** 3;
}

# Define a subroutine to print a multiplication table
sub multiplication_table {
    my ($n) = @_;
    for (my $i = 1; $i <= $n; $i++) {
        for (my $j = 1; $j <= $n; $j++) {
            printf("%3d ", $i * $j);
        }
        print "\n";
    }
}

# Define a subroutine to find the greatest common divisor of two numbers
sub gcd {
    my ($a, $b) = @_;
    return $b if $a == 0;
    return gcd($b % $a, $a);
}

# Define a subroutine to find the least common multiple of two numbers
sub lcm {
    my ($a, $b) = @_;
    return ($a * $b) / gcd($a, $b);
}

# Define a subroutine to check if a number is prime
sub is_prime {
    my ($n) = @_;
    return 1 if $n == 2;
    return 0 if $n < 2 || $n % 2 == 0;
    for (my $i = 3; $i * $i <= $n; $i += 2) {
        return 0 if $n % $i == 0;
    }
    return 1;
}

# Define a subroutine to generate a list of prime numbers up to a given number
sub prime_list {
    my ($n) = @_;
    my @primes;
    for (my $i = 2; $i <= $n; $i++) {
        push @primes, $i if is_prime($i);
    }
    return @primes;
}

# Define a subroutine to find the Fibonacci sequence of a given length
sub fibonacci {
    my ($n) = @_;
    my @fib = (0, 1);
    while (@fib.