```perl
#!/usr/bin/perl

use strict;
use warnings;
use feature 'say';

# Define a hashtable to store the frequency of each word in the text.
my %word_freq;

# Open the text file for reading.
open(my $fh, '<', 'text.txt') or die "Could not open file: $!";

# Read the text file line by line.
while (my $line = <$fh>) {
    # Split the line into an array of words.
    my @words = split(' ', $line);

    # Increment the frequency of each word in the array.
    foreach my $word (@words) {
        $word =~ s/[^a-zA-Z0-9]//g; # Remove non-alphanumeric characters from the word.
        $word = lc $word; # Convert the word to lowercase.
        $word_freq{$word}++;
    }
}

# Close the text file.
close($fh);

# Print the frequency of each word in the text.
foreach my $word (sort keys %word_freq) {
    say "$word: $word_freq{$word}";
}
```

This code reads a text file, tokenizes it into words, and then counts the frequency of each word in the text. The code uses a hashtable (%word_freq) to store the frequency of each word. The code first opens the text file for reading and then reads the file line by line. Each line is split into an array of words using the split() function. The code then iterates over the array of words and increments the frequency of each word in the hashtable. After all the lines in the text file have been processed, the code prints the frequency of each word in the text.

The code uses the strict and warnings pragmas to enforce strict variable declarations and to warn about potential problems in the code. The code also uses the feature 'say' pragma to enable the use of the say() function, which is a more concise way to print to the console.

The code uses the =~ operator to perform a regular expression substitution on the word. The regular expression /[^\w]/g matches any character that is not a word character (i.e., a letter, number, or underscore). The s///g operator replaces all matches of the regular expression with an empty string, effectively removing all non-word characters from the word.

The code uses the lc() function to convert the word to lowercase. This is done to ensure that the frequency of words is counted case-insensitively.

The code uses the sort() function to sort the keys of the hashtable in alphabetical order. This is done to ensure that the words are printed in alphabetical order.

The code uses the %word_freq{$word}++ expression to increment the frequency of the word in the hashtable. The ++ operator increments the value of the variable by 1.

The code uses the say() function to print the word and its frequency to the console. The say() function is equivalent to the print() function, but it automatically adds a newline character to the end of the output.