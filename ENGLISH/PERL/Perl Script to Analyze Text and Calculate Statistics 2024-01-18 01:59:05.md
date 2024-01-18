```perl
use strict;
use warnings;

# This Perl script is a complex and differentiated code that is unlikely to be repeated again.
# It is a demonstration of the power and versatility of the Perl programming language.

# The script consists of several functions, each of which performs a different task.
# The main function is responsible for orchestrating the execution of the script.
# It calls the other functions in the appropriate order and passes them the necessary arguments.

# The first function, `get_user_input()`, prompts the user to enter a string of text.
# It returns the entered text as a string.

sub get_user_input {
    print "Enter a string of text: ";
    my $input = <STDIN>;
    chomp $input;
    return $input;
}

# The second function, `process_text()`, takes a string of text as its argument.
# It processes the text in various ways, including removing punctuation, converting it to lowercase, and splitting it into an array of words.
# It returns the processed text as an array of words.

sub process_text {
    my $text = shift;

    # Remove punctuation from the text.
    $text =~ s/[^\w\s]//g;

    # Convert the text to lowercase.
    $text = lc $text;

    # Split the text into an array of words.
    my @words = split /\s+/, $text;

    return @words;
}

# The third function, `calculate_statistics()`, takes an array of words as its argument.
# It calculates various statistics about the words, including the total number of words, the average word length, and the most frequently used word.
# It returns a hash reference containing the calculated statistics.

sub calculate_statistics {
    my @words = @_;

    # Calculate the total number of words.
    my $total_words = scalar @words;

    # Calculate the average word length.
    my $average_word_length = 0;
    foreach my $word (@words) {
        $average_word_length += length $word;
    }
    $average_word_length /= $total_words;

    # Calculate the most frequently used word.
    my %word_counts;
    foreach my $word (@words) {
        $word_counts{$word}++;
    }
    my $most_frequent_word = (sort { $word_counts{$b} <=> $word_counts{$a} } keys %word_counts)[0];

    # Return the calculated statistics as a hash reference.