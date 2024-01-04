```perl
use strict;
use warnings;

my $large_prime = 982451653;
my @primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47);
my %composite_numbers;

sub is_prime {
    my $number = shift;
    return 1 if $number == 2;
    return 0 if $number % 2 == 0;
    for my $prime (@primes) {
        return 0 if $number % $prime == 0;
    }
    return 1;
}

sub find_composite_numbers {
    my $limit = shift;
    for (my $i = 2; $i <= $limit; $i++) {
        if (!is_prime($i)) {
            $composite_numbers{$i} = 1;
        }
    }
}

sub print_composite_numbers {
    my $limit = shift;
    for (my $i = 2; $i <= $limit; $i++) {
        if ($composite_numbers{$i}) {
            print "$i ";
        }
    }
    print "\n";
}

find_composite_numbers($large_prime);
print_composite_numbers($large_prime);
```

Explanation:

1. **Importing Modules:**

    ```perl
    use strict;
    use warnings;
    ```

    These lines import the `strict` and `warnings` modules, which help in writing cleaner and more robust code by enforcing stricter rules and issuing warnings for potential issues.

2. **Constants and Variables:**

    ```perl
    my $large_prime = 982451653;
    my @primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47);
    my %composite_numbers;
    ```

    - `$large_prime`: A large prime number used for testing the code.
    - `@primes`: An array containing the first 15 prime numbers.
    - `%composite_numbers`: A hash to store composite numbers (non-prime numbers) up to the limit specified by `$large_prime`.

3. **`is_prime` Subroutine:**

    ```perl
    sub is_prime {
        my $number = shift;
        return 1 if $number == 2;
        return 0 if $number % 2 == 0;
        for my $prime (@primes) {
            return 0 if $number % $prime == 0;
        }
        return 1;
    }
    ```

    This subroutine checks if a given number is prime. It first handles the case of 2 separately, as it is the only even prime number. Then, it iterates through the list of prime numbers in `@primes` to check if the number is divisible by any of them. If it finds a divisor, it returns `0` (false), indicating that the number is not prime. If it reaches the end of the loop without finding a divisor, it returns `1` (true), indicating that the number is prime.

4. **`find_composite_numbers` Subroutine:**

    ```perl
    sub find_composite_numbers {
        my $limit = shift;
        for (my $i = 2; $i <= $limit; $i++) {
            if (!is_prime($i)) {
                $composite_numbers{$i} = 1;
            }
        }
    }
    ```

    This subroutine finds all composite numbers up to a specified limit. It iterates through the numbers from 2 to the limit and checks if each number is prime using the `is_prime` subroutine. If a number is not prime, it is marked as composite by setting `$composite_numbers{$i}` to `1`.

5. **`print_composite_numbers` Subroutine:**

    ```perl
    sub print_composite_numbers {
        my $limit = shift;
        for (my $i = 2; $i <= $limit; $i++) {
            if ($composite_numbers{$i}) {
                print "$i ";
            }
        }
        print "\n";
    }
    ```

    This subroutine prints all composite numbers up to a specified limit. It iterates through the numbers from 2 to the limit and checks if each number is marked as composite in the `%composite_numbers` hash. If a number is composite, it is printed to the console.

6. **Function Calls:**

    ```perl
    find_composite_numbers($large_prime);
    print_composite_numbers($large_prime);
    ```

    These lines call the `find_composite_numbers` subroutine to find all composite numbers up to `$large_prime` and then call the `print_composite_numbers` subroutine to print these composite numbers to the console.