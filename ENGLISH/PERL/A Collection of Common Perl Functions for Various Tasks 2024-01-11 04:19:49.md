```perl
use strict;
use warnings;
use Data::Dumper;

my $input_string = "The quick brown fox jumps over the lazy dog";

# Function to reverse a string
sub reverse_string {
    my $string = shift;
    my @reversed_chars = reverse split //, $string;
    return join '', @reversed_chars;
}

# Function to find the longest word in a string
sub find_longest_word {
    my $string = shift;
    my @words = split /\s+/, $string;
    my $longest_word = '';
    my $longest_word_length = 0;
    foreach my $word (@words) {
        my $word_length = length($word);
        if ($word_length > $longest_word_length) {
            $longest_word = $word;
            $longest_word_length = $word_length;
        }
    }
    return $longest_word;
}

# Function to count the number of vowels in a string
sub count_vowels {
    my $string = shift;
    my $vowel_count = 0;
    my @vowels = ('a', 'e', 'i', 'o', 'u');
    foreach my $char (split //, $string) {
        if (grep {$_ eq lc($char)} @vowels) {
            $vowel_count++;
        }
    }
    return $vowel_count;
}

# Function to find the most frequent character in a string
sub find_most_frequent_character {
    my $string = shift;
    my %char_frequency;
    foreach my $char (split //, $string) {
        $char_frequency{$char}++;
    }
    my $most_frequent_char = '';
    my $highest_frequency = 0;
    foreach my $char (keys %char_frequency) {
        if ($char_frequency{$char} > $highest_frequency) {
            $most_frequent_char = $char;
            $highest_frequency = $char_frequency{$char};
        }
    }
    return $most_frequent_char;
}

# Function to generate a random password
sub generate_random_password {
    my $length = shift;
    my $chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    my @password_chars = split //, $chars;
    my $password = '';
    foreach (1..$length) {
        my $random_index = int(rand(scalar @password_chars));
        $password .= $password_chars[$random_index];
    }
    return $password;
}

# Function to calculate the factorial of a number
sub calculate_factorial {
    my $number = shift;
    my $factorial = 1;
    foreach (2..$number) {
        $factorial *= $_;
    }
    return $factorial;
}

# Function to find the greatest common divisor of two numbers
sub find_gcd {
    my ($a, $b) = @_;
    while ($b) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

# Function to find the least common multiple of two numbers
sub find_lcm {
    my ($a, $b) = @_;
    return int($a * $b / find_gcd($a, $b));
}

# Function to check if a number is prime
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

# Function to find all prime numbers up to a given number
sub find_prime_numbers {
    my $limit = shift;
    my @prime_numbers;
    foreach my $number (2..$limit) {
        if (is_prime($number)) {
            push @prime_numbers, $number;
        }
    }
    return @prime_numbers;
}

# Function to find the Fibonacci sequence up to a given number of terms
sub find_fibonacci_sequence {
    my $terms = shift;
    my @fibonacci_sequence = (0, 1);
    while (scalar @fibonacci_sequence < $terms) {
        my $next_term = $fibonacci_sequence[-1] + $fibonacci_sequence[-2];
        push @fibonacci_sequence, $next_term;
    }
    return @fibonacci_sequence;
}

# Function to find the sum of the digits of a number
sub sum_of_digits {
    my $number = shift;
    my $sum = 0;
    while ($number > 0) {
        $sum += $number % 10;
        $number = int($number / 10);
    }
    return $sum;
}

# Function to check if a number is a palindrome
sub is_palindrome {
    my $number = shift;
    my $reversed_number = reverse_string($number);
    return $number == $reversed_number;
}

# Function to find the largest palindrome made from the product of two 3-digit numbers
sub find_largest_palindrome_product {
    my $largest_palindrome = 0;
    foreach my $i (100..999) {
        foreach my $j (100..999) {
            my $product = $i * $j;
            if ($product > $largest_palindrome && is_palindrome($product)) {
                $largest_palindrome = $product;
            }
        }
    }
    return $largest_palindrome;
}

# Function to find the sum of all even numbers in the Fibonacci sequence up to a given number of terms
sub sum_of_even_fibonacci_numbers {
    my $terms = shift;
    my $sum = 0;
    my @fibonacci_sequence = find_fibonacci_sequence($terms);
    foreach my $number (@fibonacci_sequence) {
        if ($number % 2 == 0) {
            $sum += $number;
        }
    }
    return $sum;
}

# Function to find the smallest number that is divisible by all numbers from 1 to 20
sub find_smallest_divisible_number {
    my $limit = shift;
    my $smallest_divisible_number = 1;
    for (my $i = 2; $i <= $limit; $i++) {
        if ($smallest_divisible_number % $i != 0) {
            $smallest_divisible_number = lcm($smallest_divisible_number, $i);
        }
    }
    return $smallest_divisible_number;
}

# Function to find the sum of the prime numbers below a given number
sub sum_of_prime_numbers {
    my $limit = shift;
    my $sum = 0;
    foreach my $number (find_prime_numbers($limit)) {
        $sum += $number;
    }
    return $sum;
}

# Function to find the nth Fibonacci number using the Binet's formula
sub find_fibonacci_number_binet {
    my $n = shift;
    my $phi = (1 + sqrt(5)) / 2;
    my $psi = (1 - sqrt(5)) / 2;
    return int(($phi ** $n - $psi ** $n) / sqrt(5));
}

# Function to find the number of ways to climb a staircase of n stairs, where you can take either 1 or 2 steps at a time
sub count_staircase_paths {
    my $stairs = shift;
    my @dp = (0) x ($stairs + 1);
    $dp[0] = 1;
    $dp[1] = 1;
    for (my $i = 2; $i <= $stairs; $i++) {
        $dp[$i] = $dp[$i - 1] + $dp[$i - 2];
    }
    return $dp[$stairs];
}

# Function to find the longest common substring between two strings
sub find_longest_common_substring {
    my ($string1, $string2) = @_;
    my $length1 = length($string1);
    my $length2 = length($string2);
    my @dp = (0) x ($length1 + 1);
    my $longest_common_substring = '';
    my $longest_common_substring_length = 0;
    for (my $i = 1; $i <= $length1; $i++) {
