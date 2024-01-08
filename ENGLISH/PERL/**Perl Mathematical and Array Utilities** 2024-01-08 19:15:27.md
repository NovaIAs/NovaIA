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

# Define a subroutine to generate a Fibonacci sequence
sub fibonacci {
    my $length = shift;
    my @sequence = (0, 1);
    while (@sequence.scalar < $length) {
        my $next = $sequence[-1] + $sequence[-2];
        push @sequence, $next;
    }
    return @sequence;
}

# Define a subroutine to check if a number is prime
sub is_prime {
    my $number = shift;
    if ($number <= 1) {
        return 0;
    }
    for (my $i = 2; $i < $number; $i++) {
        if ($number % $i == 0) {
            return 0;
        }
    }
    return 1;
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
    return $a * $b / gcd($a, $b);
}

# Define a subroutine to generate a random number between two numbers
sub random_number {
    my $min = shift;
    my $max = shift;
    return int(rand() * ($max - $min + 1)) + $min;
}

# Define a subroutine to sort an array of numbers in ascending order
sub sort_ascending {
    my @array = @_;
    @array.sort { $a <=> $b };
    return @array;
}

# Define a subroutine to sort an array of numbers in descending order
sub sort_descending {
    my @array = @_;
    @array.sort { $b <=> $a };
    return @array;
}

# Define a subroutine to search for an element in an array
sub search {
    my $array = shift;
    my $element = shift;
    for (my $i = 0; $i < @$array; $i++) {
        if ($array->[$i] == $element) {
            return $i;
        }
    }
    return -1;
}

# Define a subroutine to reverse an array
sub reverse {
    my @array = @_;
    @array.reverse;
    return @array;
}

# Define a subroutine to join two arrays into one array
sub join {
    my @array1 = @_;
    my @array2 = @_;
    my @joined_array = (@array1, @array2);
    return @joined_array;
}

# Define a subroutine to find the intersection of two arrays
sub intersection {
    my @array1 = @_;
    my @array2 = @_;
    my @intersection = grep { $_