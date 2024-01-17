```perl
use strict;
use warnings;

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my $number = shift;
    my $result = 1;
    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to generate a list of prime numbers up to a given number
sub generate_primes {
    my $limit = shift;
    my @primes;
    for (my $i = 2; $i <= $limit; $i++) {
        my $is_prime = 1;
        for (my $j = 2; $j <= $i / 2; $j++) {
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

# Define a subroutine to find the greatest common divisor of two numbers
sub gcd {
    my $a = shift;
    my $b = shift;
    while ($b) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

# Define a subroutine to find the least common multiple of two numbers
sub lcm {
    my $a = shift;
    my $b = shift;
    return ($a * $b) / gcd($a, $b);
}

# Define a subroutine to check if a number is a palindrome
sub is_palindrome {
    my $number = shift;
    my $reversed_number = reverse $number;
    return $number == $reversed_number;
}

# Define a subroutine to generate a random number between two numbers
sub generate_random_number {
    my $min = shift;
    my $max = shift;
    return int(rand($max - $min + 1)) + $min;
}

# Define a subroutine to sort a list of numbers in ascending order
sub sort_numbers_ascending {
    my @numbers = @_;
    @numbers = sort { $a <=> $b } @numbers;
    return @numbers;
}

# Define a subroutine to sort a list of numbers in descending order
sub sort_numbers_descending {
    my @numbers = @_;
    @numbers = sort { $b <=> $a } @numbers;
    return @numbers;
}

# Define a subroutine to find the median of a list of numbers
sub median {
    my @numbers = @_;
    @numbers = sort { $a <=> $b } @numbers;
    my $length = @numbers;
    if ($length % 2 == 1) {
        return $numbers[($length - 1) / 2];
    } else {
        return ($numbers[$length / 2] + $numbers[$length / 2 - 1]) / 2;
    }
}

# Define a subroutine to find the mode of a list of numbers
sub mode