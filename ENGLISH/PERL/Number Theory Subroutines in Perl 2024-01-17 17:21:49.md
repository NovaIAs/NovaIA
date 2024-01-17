```perl
use strict;
use warnings;

# Define a subroutine to calculate the Fibonacci sequence up to a given number
sub fibonacci {
    my $n = shift;
    my @fib = (0, 1);
    while ($fib[-1] < $n) {
        push @fib, $fib[-2] + $fib[-1];
    }
    return @fib;
}

# Define a subroutine to check if a number is prime
sub is_prime {
    my $n = shift;
    return 1 if $n == 2;
    return 0 if $n % 2 == 0;
    my $sqrt = sqrt($n);
    for (my $i = 3; $i <= $sqrt; $i += 2) {
        return 0 if $n % $i == 0;
    }
    return 1;
}

# Define a subroutine to find the largest prime factor of a number
sub largest_prime_factor {
    my $n = shift;
    my $largest_prime_factor = 1;
    for (my $i = 2; $i <= $n; $i++) {
        if ($n % $i == 0 && is_prime($i)) {
            $largest_prime_factor = $i;
        }
    }
    return $largest_prime_factor;
}

# Define a subroutine to find the sum of the digits of a number
sub sum_of_digits {
    my $n = shift;
    my $sum = 0;
    while ($n > 0) {
        $sum += $n % 10;
        $n = int($n / 10);
    }
    return $sum;
}

# Define a subroutine to find the factorial of a number
sub factorial {
    my $n = shift;
    my $factorial = 1;
    for (my $i = 1; $i <= $n; $i++) {
        $factorial *= $i;
    }
    return $factorial;
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
    return $a * $b / gcd($a, $b);
}

# Define a subroutine to find the Pythagorean triples up to a given number
sub pythagorean_triples {
    my $n = shift;
    my @triples;
    for (my $a = 1; $a <= $n; $a++) {
        for (my $b = $a + 1; $b <= $n; $b++) {
            my $c = sqrt($a**2 + $b**2);
            if ($c == int($c) && $c <= $n) {
                push @triples, [$a, $b, $c];
            }
        }
    }
    return @triples;
}

# Define a subroutine to check if a number is pandigital
sub is_pandigital {
    my $n = shift;
    my $digits = join "", sort split //, $n;
    return $digits eq "123456789";
}

# Define a subroutine to find the pandigital prime numbers up to a given number
sub pandigital_primes {
    my $n = shift;
    my @pandigital_primes