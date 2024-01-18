```perl
use strict;
use warnings;

# Define a complex regular expression to match various patterns
my $regex = qr/(?x-ism)
    # Match a sequence of one or more digits
    \d+

    # Match an optional decimal point followed by one or more digits
    (\.\d+)?

    # Match an optional scientific notation exponent
    (e[+-]?\d+)?

    # Match an optional currency symbol before the number
    (\$|€|£)?

    # Match an optional percentage sign after the number
    (%)?
/;

# List of complex data structures to process
my @data = (
    '123',
    '456.789',
    '1.234e+5',
    '$12.34',
    '€456.78',
    '£1,234.56',
    '100%',
);

foreach my $item (@data) {
    # Check if the item matches the regular expression
    if ($item =~ $regex) {
        print "The item '$item' matches the complex regular expression.\n";
    } else {
        print "The item '$item' does not match the complex regular expression.\n";
    }
}
```

Explanation:

1. We start by importing the `strict` and `warnings` modules to enable stricter error checking and warnings.

2. We define a complex regular expression using the `qr//` syntax. The `/x-ism` flags are used to enable extended regular expression syntax, ignore whitespace and comments in the pattern, and make the pattern case-insensitive.

3. The regular expression pattern is enclosed in parentheses and contains several subpatterns:

   - `\d+`: Matches one or more digits (numbers).
   - `(\.\d+)?`: Matches an optional decimal point followed by one or more digits (for decimal numbers).
   - `(e[+-]?\d+)?`: Matches an optional scientific notation exponent, which consists of the letter "e" followed by an optional sign (+ or -) and one or more digits.
   - `(\$|€|£)?`: Matches an optional currency symbol ($, €, or £) before the number.
   - `(%)?`: Matches an optional percentage sign (%) after the number.

4. We create an array `@data` containing various complex data items to be processed. These items include numeric values, currency values, percentages, and numbers in scientific notation.

5. We use a `foreach` loop to iterate through each item in the `@data` array.

6. Inside the loop, we use the `=~` operator to check if the current item matches the complex regular expression. If it matches, we print a message indicating that it matches the regex. Otherwise, we print a message indicating that it does not match.

This code demonstrates how to use a complex regular expression to match various patterns in different data items. It highlights the flexibility and power of regular expressions for data validation and manipulation tasks.