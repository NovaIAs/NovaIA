Here is a complex Perl code that performs various operations:

```
#!usr/bin/perl

use strict;
use warnings;

# Define a subroutine to generate a random number between two values
sub random_number {
    my ($min, $max) = @_;
    return int(rand() * ($max - $min + 1)) + $min;
}

# Define a subroutine to generate a random string of a specific length
sub random_string {
    my $length = shift;
    my @chars = ('a'..'z', 'A'..'Z', 0..9);
    my $string = join '', map { $chars[random_number(0, @chars - 1)] } 1..$length;
    return $string;
}

# Define a subroutine to print a table
sub print_table {
    my $table = shift;
    my $width = 0;
    foreach my $row (@$table) {
        foreach my $cell (@$row) {
            $width = length($cell) if length($cell) > $width;
        }
    }

    foreach my $row (@$table) {
        foreach my $cell (@$row) {
            printf "%${width}s ", $cell;
        }
        print "\n";
    }
}

# Generate a 2D array of random numbers
my $array = [[random_number(1, 10) for 1..3] for 1..3];

# Print the array using the print_table subroutine
print_table($array);

# Generate a random string of length 10
my $string = random_string(10);

# Print the random string
print "\nRandom string: $string\n";

# Find the largest prime factor of a number
my $number = 60;
my $largest_prime_factor = 1;

for (my $i = 2; $i <= $number; $i++) {
    if ($number % $i == 0) {
        my $is_prime = 1;
        for (my $j = 2; $j < $i; $j++) {
            if ($i % $j == 0) {
                $is_prime = 0;
                last;
            }
        }

        if ($is_prime && $i > $largest_prime_factor) {
            $largest_prime_factor = $i;
        }
    }
}

# Print the largest prime factor
print "\nLargest prime factor of $number: $largest_prime_factor\n";

# Define a subroutine to calculate the factorial of a number
sub factorial {
    my $number = shift;
    my $factorial = 1;

    for (my $i = 1; $i <= $number; $i++) {
        $factorial *= $i;
    }

    return $factorial;
}

# Calculate the factorial of a number
my $factorial_of_5 = factorial(5);

# Print the factorial
print "\nFactorial of 5: $factorial_of_5\n";

# Define a subroutine to check if a number is a palindrome
sub is_palindrome {
    my $number = shift;
    my $reversed_number = reverse $number;
    return $number == $reversed_number;
}

# Check if a number is a palindrome
my $number = 12321;
my $is_palindrome = is_palindrome($number);

# Print the result
print "\nIs $number a palindrome? ", $is_palindrome ? "Yes" : "No", "\n";
```

Explanation:

1. First, we define several subroutines, including `random_number`, `random_string`, `print_table`, `factorial`, and `is_palindrome`. These subroutines perform specific tasks such as generating random numbers, random strings, printing tables, calculating factorials, and checking if a number is a palindrome.

2. We generate a 2D array of random numbers and print it using the `print_table` subroutine.

3. We generate a random string of length 10 and print it.

4. We find the largest prime factor of a given number and print it.

5. We calculate the factorial of a number and print it.

6. We check if a given number is a palindrome and print the result.