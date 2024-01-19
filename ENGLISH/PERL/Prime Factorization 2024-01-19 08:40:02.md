```perl
use strict;
use warnings;

# Define the main program
my $main = sub {
    # Get the input from the user
    print "Enter a number: ";
    my $number = <STDIN>;

    # Check if the input is a valid number
    if ($number !~ /^\d+$/) {
        print "Invalid input. Please enter a number.\n";
        return;
    }

    # Convert the input to an integer
    my $number_int = int($number);

    # Create a hash to store the prime factors of the number
    my %prime_factors;

    # Find the prime factors of the number
    my $divisor = 2;
    while ($number_int > 1) {
        if ($number_int % $divisor == 0) {
            # The divisor is a prime factor of the number
            $prime_factors{$divisor}++;

            # Divide the number by the divisor
            $number_int /= $divisor;
        } else {
            # The divisor is not a prime factor of the number
            $divisor++;
        }
    }

    # Print the prime factors of the number
    print "Prime factors of $number: ";
    foreach my $prime_factor (sort keys %prime_factors) {
        print "$prime_factor^$prime_factors{$prime_factor} ";
    }
    print "\n";
};

# Run the main program
$main->();
```

Explanation:

1. The `use strict;` and `use warnings;` statements are used to enforce strict programming practices and enable warnings for potential problems in the code.

2. The `$main` subroutine is defined as the main program. This subroutine will be executed when the Perl script is run.

3. Inside the `$main` subroutine, the user is prompted to enter a number using the `print` statement. The input is then read from the standard input using the `<>` operator and stored in the `$number` variable.

4. The `if` statement checks if the input is a valid number. If the input does not match the regular expression `^\d+$`, which checks for a sequence of digits, an error message is printed and the program returns.

5. The `int()` function is used to convert the input string to an integer, which is stored in the `$number_int` variable.

6. A hash (associative array) called `%prime_factors` is created to store the prime factors of the number.

7. A `while` loop is used to find the prime factors of the number. The loop starts with the divisor set to 2 and continues until the number is reduced to 1.

8. Inside the loop, the `if` statement checks if the number is divisible by the divisor. If it is, the divisor is a prime factor of the number and is added to the `%prime_factors` hash. The number is then divided by the divisor to remove the prime factor.

9. If the number is not divisible by the divisor, the divisor is incremented and the loop continues.

10. After the loop, the `foreach` loop is used to iterate over the keys of the `%prime_factors` hash, which are the prime factors of the number. For each prime factor, the `print` statement prints the prime factor and its exponent, which is the value associated with the key in the hash.

11. Finally, the `\n` character is printed to start a new line.