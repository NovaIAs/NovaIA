Here is a complex and differentiated code in Perl:

```
use strict;
use warnings;

my $input_string = "This is an example of a complex and differentiated Perl code.";

# Split the input string into an array of words
my @words = split /\s+/, $input_string;

# Create a hash to store the frequency of each word
my %word_frequency;

# Iterate over the array of words and count the frequency of each word
foreach my $word (@words) {
    $word_frequency{$word}++;
}

# Print the frequency of each word
foreach my $word (sort keys %word_frequency) {
    print "$word: $word_frequency{$word}\n";
}

# Create a subroutine to calculate the average word length
sub average_word_length {
    my $total_length = 0;
    my $num_words = 0;

    # Iterate over the array of words and calculate the total length and number of words
    foreach my $word (@words) {
        $total_length += length($word);
        $num_words++;
    }

    # Calculate and return the average word length
    return $total_length / $num_words;
}

# Print the average word length
print "Average word length: ", average_word_length(), "\n";
```

Explanation:

* The code starts by importing the `strict` and `warnings` modules, which help to enforce good programming practices and catch potential errors.
* The `$input_string` variable is assigned a sample text string.
* The `split` function is used to split the input string into an array of words, and the resulting array is stored in the `@words` array.
* A hash (`%word_frequency`) is created to store the frequency of each word.
* A `foreach` loop is used to iterate over the array of words and increment the frequency count for each word in the hash.
* Another `foreach` loop is used to print the frequency of each word, sorted in alphabetical order.
* A subroutine named `average_word_length` is defined to calculate the average length of the words in the input string.
* The subroutine iterates over the array of words, calculating the total length and number of words.
* The average word length is calculated by dividing the total length by the number of words.
* The average word length is printed using the `print` function.

This code demonstrates the use of various Perl features, including regular expressions, hashes, subroutines, and loops, to perform a complex and differentiated task. The code is also well-commented and easy to understand, making it a good example of Perl programming.