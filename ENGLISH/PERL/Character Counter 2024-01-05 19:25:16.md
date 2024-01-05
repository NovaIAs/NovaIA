```perl
use strict;
use warnings;
use feature 'say';

my $input = "Hello, world!";

# Split the input string into an array of characters
my @chars = split //, $input;

# Create a hash to store the character counts
my %char_counts;

# Iterate over the array of characters and increment the count for each character
foreach my $char (@chars) {
  $char_counts{$char}++;
}

# Print the character counts
foreach my $char (sort keys %char_counts) {
  say "$char: $char_counts{$char}";
}
```

This code takes a string as input and counts the number of occurrences of each character in the string. It uses a hash to store the character counts, and then prints the counts in alphabetical order.

Here is a more detailed explanation of the code:

* The `use strict` and `use warnings` statements are used to enable strict checking of variable declarations and to enable warnings about potential problems in the code.
* The `use feature 'say'` statement enables the use of the `say` function, which is a more concise way to print a value to the console.
* The `my $input = "Hello, world!";` statement declares a variable named `$input` and assigns it the value of the string "Hello, world!".
* The `my @chars = split //, $input;` statement splits the input string into an array of characters. The `//` delimiter tells the `split` function to split the string on every character.
* The `my %char_counts;` statement declares a hash named `%char_counts`. A hash is a data structure that stores key-value pairs. In this case, the keys will be the characters in the input string, and the values will be the counts of each character.
* The `foreach my $char (@chars) { $char_counts{$char}++; }` statement iterates over the array of characters and increments the count for each character in the hash.
* The `foreach my $char (sort keys %char_counts) { say "$char: $char_counts{$char}"; }` statement iterates over the keys of the hash (which are the characters in the input string) in alphabetical order and prints the character and its count.