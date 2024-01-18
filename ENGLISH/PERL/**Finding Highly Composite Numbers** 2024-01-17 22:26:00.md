```perl
use strict;
use warnings;
use feature 'say';
use List::AllUtils 'any';

sub is_prime {
    my $n = shift;
    return 1 if $n <= 1;
    return 0 if any { !($n % $_) } 2 .. int sqrt($n);
    return 1;
}

sub prime_factors {
    my $n = shift;
    my @factors;
    while ($n > 1) {
        for my $i (2 .. $n) {
            if ($n % $i == 0 && is_prime($i)) {
                push @factors, $i;
                $n /= $i;
                last;
            }
        }
    }
    return @factors;
}

sub count_factors {
    my $n = shift;
    my %factors;
    while ($n > 1) {
        for my $i (2 .. $n) {
            if ($n % $i == 0 && is_prime($i)) {
                $factors{$i}++;
                $n /= $i;
                last;
            }
        }
    }
    return scalar keys %factors;
}

sub highly_composite {
    my $limit = shift;
    my %composites;
    for (my $i = 2; $i <= $limit; $i++) {
        $composites{$i} = count_factors($i);
    }
    my @highly_composite = grep { $composites{$_} > $composites{$_ - 1} } 2 .. $limit;
    return @highly_composite;
}

say join ', ', highly_composite(100);
```

Explanation:

1. **`is_prime` Function**: This function checks if a given number is prime. It uses a simple loop to check divisibility by numbers from 2 to the square root of the given number. If any of these numbers evenly divides the given number, it returns `0` (indicating it's not prime); otherwise, it returns `1` (indicating it's prime).

2. **`prime_factors` Function**: This function finds all the prime factors of a given number. It repeatedly divides the number by the smallest prime factor it can find until the number becomes 1. The prime factors are stored in an array and returned.

3. **`count_factors` Function**: This function counts the number of prime factors of a given number. It uses a similar approach as the `prime_factors` function, but instead of storing the factors, it increments a counter for each prime factor found. The final count is returned.

4. **`highly_composite` Function**: This function finds a list of highly composite numbers up to a given limit. A highly composite number is one with more divisors than the previous number. The function uses the `count_factors` function to calculate the number of factors for each number and stores them in a hash. It then finds the numbers with more factors than their predecessors and returns them as an array.

5. **Usage**: The code calculates and prints the list of highly composite numbers up to 100. The output will show the numbers that have more divisors than the previous number in the sequence.