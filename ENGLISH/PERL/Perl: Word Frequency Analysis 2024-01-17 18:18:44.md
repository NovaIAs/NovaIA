#!/usr/bin/perl

use strict;
use warnings;

use Data::Dumper;

my $input = "This is a sample string to be processed.";

# This regular expression matches words that start with a capital letter.
my $pattern = qr/[A-Z]\w+/g;

# This hash will store the frequency of each word in the input string.
my %word_counts;

# This loop iterates over each word in the input string.
foreach my $word ($input =~ $pattern) {
  # This line increments the count of the current word in the hash.
  $word_counts{$word}++;
}

# This line prints the hash containing the frequency of each word.
print Data::Dumper->Dump(\%word_counts, [qw/sort/]);

# This line prints the total number of words in the input string.
my $total_words = scalar keys %word_counts;
print "Total words: $total_words\n";

# This line prints the most frequently occurring word in the input string.
my ($most_frequent_word) = sort { $word_counts{$b} <=> $word_counts{$a} } keys %word_counts;
print "Most frequent word: $most_frequent_word\n";

# This line prints the least frequently occurring word in the input string.
my ($least_frequent_word) = sort { $word_counts{$a} <=> $word_counts{$b} } keys %word_counts;
print "Least frequent word: $least_frequent_word\n";