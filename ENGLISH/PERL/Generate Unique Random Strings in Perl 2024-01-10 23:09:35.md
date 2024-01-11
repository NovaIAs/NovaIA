```perl
use strict;
use warnings;
use Data::Dumper;

# Define a subroutine to generate a random string of a given length
sub generate_random_string {
    my $length = shift;
    my @chars = ('a'..'z', 'A'..'Z', '0'..'9');
    my $string = '';
    for (1..$length) {
        $string .= $chars[rand @chars];
    }
    return $string;
}

# Define a subroutine to generate a list of unique random strings of a given length
sub generate_unique_random_strings {
    my $length = shift;
    my $count = shift;
    my @strings;
    while (@strings < $count) {
        my $string = generate_random_string($length);
        push @strings, $string unless grep { $_ eq $string } @strings;
    }
    return @strings;
}

# Generate a list of 10 unique random strings of length 10
my @strings = generate_unique_random_strings(10, 10);

# Print the list of strings
print Data::Dumper->Dump(\@strings, [qw/sort/]);
```

Explanation:

1. **`use strict;` and `use warnings;`:** These lines enable strict mode and warnings in Perl. Strict mode helps catch common programming errors, and warnings help identify potential problems in the code.

2. **`use Data::Dumper;`:** This line imports the Data::Dumper module, which is used to print the list of strings in a human-readable format.

3. **`sub generate_random_string { ... }`:** This is a subroutine that generates a random string of a given length. It takes a single parameter, `$length`, which specifies the length of the random string to generate.

4. **`my @chars = ('a'..'z', 'A'..'Z', '0'..'9');`:** This line creates an array `@chars` containing all lowercase letters, uppercase letters, and digits. This array is used to generate the random strings.

5. **`my $string = '';`:** This line initializes a variable `$string` to an empty string. This variable will be used to store the generated random string.

6. **`for (1..$length) { ... }`:** This is a loop that iterates `$length` times. In each iteration, a random character is selected from the `@chars` array and appended to the `$string` variable.

7. **`return $string;`:** This line returns the generated random string as the result of the subroutine.

8. **`sub generate_unique_random_strings { ... }`:** This is a subroutine that generates a list of unique random strings of a given length. It takes two parameters: `$length`, which specifies the length of each random string, and `$count`, which specifies the number of random strings to generate.

9. **`my @strings;`:** This line initializes an array `@strings` to an empty array. This array will be used to store the generated unique random strings.

10. **`while (@strings < $count) { ... }`:** This is a loop that continues until the `@strings` array contains `$count` unique random strings.

11. **`my $string = generate_random_string($length);`:** This line generates a random string of length `$length` using the `generate_random_string` subroutine.

12. **`push @strings, $string unless grep { $_ eq $string } @strings;`:** This line adds the generated random string `$string` to the `@strings` array if it is unique (i.e., not already present in the array). The `grep` function is used to check if `$string` is already present in the array.

13. **`return @strings;`:** This line returns the list of unique random strings as the result of the subroutine.

14. **`my @strings = generate_unique_random_strings(10, 10);`:** This line calls the `generate_unique_random_strings` subroutine to generate a list of 10 unique random strings of length 10.

15. **`print Data::Dumper->Dump(\@strings, [qw/sort/]);`:** This line prints the list of unique random strings using the `Data::Dumper` module. The `[qw/sort/]` argument sorts the strings before printing them.