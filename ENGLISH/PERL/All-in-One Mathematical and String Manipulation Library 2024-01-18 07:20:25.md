```perl
use strict;
use warnings;

# This Perl script is designed to perform a variety of complex and differentiated tasks, ensuring that it is unlikely to be repeated again. The code is written in English for clarity and ease of understanding.

# Define a subroutine to calculate the factorial of a non-negative integer
sub factorial {
    my $number = shift;
    my $result = 1;
    for (my $i = 1; $i <= $number; $i++) {
        $result *= $i;
    }
    return $result;
}

# Define a subroutine to generate a random number within a specified range
sub generate_random_number {
    my ($min, $max) = @_;
    my $range = $max - $min + 1;
    return int(rand($range)) + $min;
}

# Define a subroutine to print a pyramid of numbers
sub print_pyramid {
    my $height = shift;
    for (my $i = 1; $i <= $height; $i++) {
        my $spaces = $height - $i;
        print " " x $spaces;
        for (my $j = 1; $j <= $i; $j++) {
            print "$j ";
        }
        print "\n";
    }
}

# Define a subroutine to check if a number is prime
sub is_prime {
    my $number = shift;
    if ($number <= 1) {
        return 0;
    }
    for (my $i = 2; $i <= int(sqrt($number)); $i++) {
        if ($number % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# Define a subroutine to find the greatest common divisor (GCD) of two numbers
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        my $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

# Define a subroutine to find the least common multiple (LCM) of two numbers
sub lcm {
    my ($a, $b) = @_;
    return int(($a * $b) / gcd($a, $b));
}

# Define a subroutine to print a table of prime numbers up to a specified limit
sub print_prime_table {
    my $limit = shift;
    print "Prime numbers up to $limit:\n";
    my $count = 0;
    for (my $i = 2; $i <= $limit; $i++) {
        if (is_prime($i)) {
            print "$i ";
            $count++;
            if ($count % 10 == 0) {
                print "\n";
            }
        }
    }
    print "\n";
}

# Define a subroutine to generate a random password of a specified length
sub generate_password {
    my $length = shift;
    my $characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()';
    my $password = '';
    for (my $i = 0; $i < $length; $i++) {
        my $index = generate_random_number(0, length($characters) - 1);
        $password .= substr($characters, $index, 1);
    }
    return $password;
}

# Define a subroutine to convert a decimal number to its binary representation
sub decimal_to_binary {
    my $number = shift;
    my $binary = '';
    while ($number > 0) {
        $binary = ($number % 2) . $binary;
        $number = int($number / 2);
    }
    return $binary;
}

# Define a subroutine to convert a binary number to its decimal representation
sub binary_to_decimal {
    my $binary = shift;
    my $decimal = 0;
    my $power = 1;
    for (my $i = length($binary) - 1; $i >= 0; $i--) {
        $decimal += substr($binary, $i, 1) * $power;
        $power *= 2;
    }
    return $decimal;
}

# Define a subroutine to find the sum of the digits of a non-negative integer
sub sum_of_digits {
    my $number = shift;
    my $sum = 0;
    while ($number > 0) {
        $sum += $number % 10;
        $number = int($number / 10);
    }
    return $sum;
}

# Define a subroutine to find the product of the digits of a non-negative integer
sub product_of_digits {
    my $number = shift;
    my $product = 1;
    while ($number > 0) {
        $product *= $number % 10;
        $number = int($number / 10);
    }
    return $product;
}

# Define a subroutine to find the reverse of a string
sub reverse_string {
    my $str = shift;
    my $reversed = '';
    for (my $i = length($str) - 1; $i >= 0; $i--) {
        $reversed .= substr($str, $i, 1);
    }
    return $reversed;
}

# Define a subroutine to check if a string is a palindrome
sub is_palindrome {
    my $str = shift;
    $str =~ s/[^A-Za-z0-9]//g;
    $str = lc($str);
    return ($str eq reverse_string($str));
}

# Define a subroutine to find the longest common substring of two strings
sub longest_common_substring {
    my ($str1, $str2) = @_;
    my $lcs = '';
    for (my $i = 0; $i < length($str1); $i++) {
        for (my $j = $i + 1; $j <= length($str1); $j++) {
            my $substring = substr($str1, $i, $j - $i);
            if (index($