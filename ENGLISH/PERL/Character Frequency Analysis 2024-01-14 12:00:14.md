```perl
use strict;
use warnings;
use feature 'say';

my $input = 'This is a sample text.';

# Create a hash to store the frequency of each character.
my %char_freq;

# Iterate over each character in the input string.
for my $char (split //, $input) {
    # Increment the frequency of the current character.
    $char_freq{$char}++;
}

# Find the character with the highest frequency.
my $max_freq = 0;
my $max_char;

foreach my $char (keys %char_freq) {
    if ($char_freq{$char} > $max_freq) {
        $max_freq = $char_freq{$char};
        $max_char = $char;
    }
}

# Print the character with the highest frequency.
say "The character with the highest frequency is '$max_char'.";

# Find the character with the lowest frequency.
my $min_freq = $max_freq;
my $min_char;

foreach my $char (keys %char_freq) {
    if ($char_freq{$char} < $min_freq) {
        $min_freq = $char_freq{$char};
        $min_char = $char;
    }
}

# Print the character with the lowest frequency.
say "The character with the lowest frequency is '$min_char'.";

# Create a histogram of the character frequencies.
my $histogram = '';

foreach my $char (sort keys %char_freq) {
    $histogram .= "$char: ";
    for (1 .. $char_freq{$char}) {
        $histogram .= '*';
    }
    $histogram .= "\n";
}

# Print the histogram.
say "Histogram of character frequencies:\n$histogram";
```

This code performs a frequency analysis of a given text. It counts the number of occurrences of each character in the text and then finds the character with the highest and lowest frequency. It also creates a histogram of the character frequencies.

Here is a breakdown of the code:

* The `use` statements at the beginning of the code import the necessary Perl modules.
* The `my $input` variable stores the input text.
* The `%char_freq` hash is used to store the frequency of each character in the input text.
* The `for` loop iterates over each character in the input text and increments the frequency of the current character in the `%char_freq` hash.
* The `foreach` loop finds the character with the highest and lowest frequency.
* The `say` statement prints the character with the highest and lowest frequency.
* The `my $histogram` variable stores the histogram of the character frequencies.
* The `foreach` loop creates the histogram by iterating over the characters in the input text and adding a `*` character for each occurrence of the character.
* The `say` statement prints the histogram.