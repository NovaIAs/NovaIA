```perl
use strict;
use warnings;

my $input_string = "This is a string with spaces and special characters!";

# Split the input string into an array of words
my @words = split /\s+/, $input_string;

# Create a hash to store the frequency of each word
my %word_freq;

# Iterate over the array of words and increment the frequency count for each word
foreach my $word (@words) {
    $word_freq{$word}++;
}

# Sort the hash keys (which are the words) in alphabetical order
my @sorted_words = sort keys %word_freq;

# Print the frequency of each word
foreach my $word (@sorted_words) {
    print "$word: $word_freq{$word}\n";
}
```

Explanation:

1. `use strict;` and `use warnings;`: These are commonly used to enable stricter error checking and warnings in Perl programs.

2. `my $input_string = "This is a string with spaces and special characters!";`: This line assigns a string value to the variable `$input_string`. The string contains spaces and special characters.

3. `my @words = split /\s+/, $input_string;`: The `split` function is used to split the input string into an array of words based on the regular expression /\s+/. The regular expression `\s+` matches one or more whitespace characters (spaces, tabs, newlines, etc.). This results in an array of words where each word is a separate element in the array.

4. `my %word_freq;`: This line creates an empty hash (associative array) called `%word_freq`. This hash will be used to store the frequency of each word in the input string.

5. `foreach my $word (@words) { $word_freq{$word}++; }`: This is a foreach loop that iterates over each element (word) in the `@words` array. For each word, it increments the frequency count in the `%word_freq` hash. If a word is not yet present in the hash, it is automatically added with a frequency of 1.

6. `my @sorted_words = sort keys %word_freq;`: This line sorts the keys (which are the words) of the `%word_freq` hash in alphabetical order using the `sort` function. The result is an array of sorted words.

7. `foreach my $word (@sorted_words) { print "$word: $word_freq{$word}\n"; }`: Finally, this loop iterates over the sorted list of words and prints the word along with its frequency count.

When you run this Perl program, it will split the input string into words, count the frequency of each word, and print the words and their frequencies in alphabetical order.