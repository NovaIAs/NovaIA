```perl
use strict;
use warnings;

# This Perl code is a complex and differentiated program that performs various operations.

# 1. Define a subroutine to calculate the factorial of a number
sub factorial {
    my ($n) = @_;
    my $result = 1;
    for (my $i = 2; $i <= $n; $i++) {
        $result *= $i;
    }
    return $result;
}

# 2. Function to calculate the greatest common divisor (GCD) of two numbers
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        my $t = $b;
        $b = $a % $b;
        $a = $t;
    }
    return $a;
}

# 3. Function to print a multiplication table for a given number
sub print_multiplication_table {
    my ($n) = @_;
    print "Multiplication Table for $n:\n";
    for (my $i = 1; $i <= 10; $i++) {
        print "$n x $i = ", $n * $i, "\n";
    }
}

# 4. Function to calculate the sum of digits in a number
sub sum_of_digits {
    my ($n) = @_;
    my $sum = 0;
    while ($n) {
        $sum += $n % 10;
        $n = int($n / 10);
    }
    return $sum;
}

# 5. Function to check if a number is prime
sub is_prime {
    my ($n) = @_;
    if ($n <= 1) {
        return 0;
    }
    for (my $i = 2; $i <= sqrt($n); $i++) {
        if ($n % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# 6. Function to print a right triangle of asterisks
sub print_right_triangle {
    my ($n) = @_;
    for (my $i = 1; $i <= $n; $i++) {
        for (my $j = 1; $j <= $i; $j++) {
            print "*";
        }
        print "\n";
    }
}

# 7. Function to print a diamond shape of asterisks
sub print_diamond {
    my ($n) = @_;
    my $mid = int(($n + 1) / 2);

    # Upper half
    for (my $i = 1; $i <= $mid; $i++) {
        for (my $j = 1; $j <= $mid - $i; $j++) {
            print " ";
        }
        for (my $k = 1; $k <= 2 * $i - 1; $k++) {
            print "*";
        }
        print "\n";
    }

    # Lower half
    for (my $i = $mid - 1; $i >= 1; $i--) {
        for (my $j = 1; $j <= $mid - $i; $j++) {
            print " ";
        }
        for (my $k = 1; $k <= 2 * $i - 1; $k++) {
            print "*";
        }
        print "\n";
    }
}

# 8. Function to print the Fibonacci sequence up to a given number
sub print_fibonacci {
    my ($n) = @_;
    my @fib = (0, 1);
    while ($fib[-1] < $n) {
        my $next = $fib[-2] + $fib[-1];
        push @fib, $next;
    }
    print "Fibonacci sequence up to $n:\n";
    print join(", ", @fib), "\n";
}

# 9. Function to perform matrix multiplication
sub matrix_multiplication {
    my ($m1, $m2) = @_;
    my $result = [];

    for (my $i = 0; $i < scalar @$m1; $i++) {
        my @row = ();
        for (my $j = 0; $j < scalar @{$m2->[0]}; $j++) {
            my $sum = 0;
            for (my $k = 0; $k < scalar @$m2; $k++) {
                $sum += $m1->[$i][$k] * $m2->[$k][$j];
            }
            push @row, $sum;
        }
        push @$result, \@row;
    }

    return $result;
}

# 10. Function to find the roots of a quadratic equation
sub quadratic_roots {
    my ($a, $b, $c) = @_;
    my $discriminant = $b ** 2 - 4 * $a * $c;

    if ($discriminant < 0) {
        print "No real roots.\n";
        return;
    }

    my $root1 = (-$b + sqrt($discriminant)) / (2 * $a);
    my $root2 = (-$b - sqrt($discriminant)) / (2 * $a);

    print "Roots of the quadratic equation:\n";
    print "x1 = ", $root1, "\n";
    print "x2 = ", $root2, "\n";
}

# 11. Function to check if a string is a palindrome
sub is_palindrome {
    my ($str) = @_;
    $str =~ s/[^a-zA-Z0-9]//g;
    $str = lc($str);
    return $str eq reverse $str;
}

# 12. Function to find the longest common substring between two strings
sub longest_common_substring {
    my ($str1, $str2) = @_;

    my $len1 = length($str1);
    my $len2 = length($str2);

    my $lcs_length = 0;
    my $lcs_start = 0;

    for (my $i = 0; $i < $len1; $i++) {
        for (my $j = 0; $j < $len2; $j++) {
            my $k = 0;
            while ($i + $k < $len1 && $j + $k < $len2 && $str1[$i + $k] eq $str2[$j + $k]) {
                $k++;
            }
            if ($k > $lcs_length) {
                $lcs_length = $k;
                $lcs_start = $i;
            }
        }
    }

    return substr($str1, $lcs_start, $lcs_length);
}

# Example usage of the functions
my $result = factorial(5);
print "Factorial of 5: $result\n";

my $gcd = gcd(12, 18);
print "GCD of 12 and 18: $gcd\n";

print_multiplication_table(7);

my $sum = sum_of_digits(12345);
print "Sum of digits in 12345: $sum\n";

if (is_prime(11)) {
    print "11 is a prime number.\n";
} else {
    print "11 is not a prime number.\n";
}

print_right_triangle(5);

print_diamond(7);

print_fibonacci(20);

my $matrix1 = [
    [1, 2, 3],
    [4, 5, 6]
];
my $matrix2 = [
    [7, 8],
    [9, 10],
    [11, 12]
];
my $result_matrix = matrix_multiplication($matrix1, $matrix2);
print "Result of matrix multiplication:\n";
for my $row (@$result_matrix) {
    print join(" ", @$row), "\n";
}

quadratic_roots(1, -5, 6);

if (is_palindrome("racecar")) {
    print "racecar is a palindrome.\n";
} else {
    print "racecar is not a palindrome.\n";
}

my $lcs = longest_common_substring("ABCDFG", "ACDFG");
print "Longest common substring: $lcs\n";
```

This Perl code performs various operations, including:

- Calculating the factorial of a number
- Finding the greatest common divisor (GCD) of two numbers
- Printing a multiplication table for a given number
- Calculating the sum of digits in a number
- Checking if a number is prime
- Printing a right triangle of asterisks
- Printing a diamond shape of asterisks
- Printing the Fibonacci sequence up to a given number
- Performing matrix multiplication
- Finding the roots of a quadratic equation
- Checking if a string is a palindrome
- Finding the longest common substring between two strings

The code provides comprehensive examples of mathematical calculations, string manipulation, and data structures in Perl. It demonstrates a diverse range of programming concepts and techniques, making it a complex and differentiated code that is unlikely to be repeated in the same manner.