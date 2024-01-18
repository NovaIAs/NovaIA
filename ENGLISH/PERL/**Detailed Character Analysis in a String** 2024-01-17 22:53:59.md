```
use strict;
use warnings;
use List::MoreUtils qw(first_index);
use Data::Dumper;

my $input = "This is a test string";

# Create a hash to store the counts of each character in the input string
my %char_counts;
foreach my $char (split //, $input) {
  $char_counts{$char}++;
}

# Find the character with the highest count
my $max_count = 0;
my $most_frequent_char;
foreach my $char (keys %char_counts) {
  if ($char_counts{$char} > $max_count) {
    $max_count = $char_counts{$char};
    $most_frequent_char = $char;
  }
}

# Find the character with the lowest count
my $min_count = $max_count;
my $least_frequent_char;
foreach my $char (keys %char_counts) {
  if ($char_counts{$char} < $min_count) {
    $min_count = $char_counts{$char};
    $least_frequent_char = $char;
  }
}

# Find the index of the first occurrence of the most frequent character
my $first_index = first_index { $_ eq $most_frequent_char } split //, $input;

# Find the index of the last occurrence of the least frequent character
my $last_index = rindex $input, $least_frequent_char;

# Print the results
print "Most frequent character: $most_frequent_char (count: $max_count)\n";
print "Least frequent character: $least_frequent_char (count: $min_count)\n";
print "First index of most frequent character: $first_index\n";
print "Last index of least frequent character: $last_index\n";

# Print the character counts hash
print "\nCharacter Counts:\n";
print Data::Dumper->Dump(\%char_counts, [qw/sort/]);
```

**Explanation:**

This Perl code performs a detailed analysis of a given input string. It calculates various statistics related to the characters in the string and prints the results.

1. **Character Counts:** It uses a hash (%char_counts) to store the counts of each character in the input string. This is done using a foreach loop that iterates over each character in the string and increments the corresponding count in the hash.

2. **Most Frequent Character:** It finds the character with the highest count in the hash. It does this by iterating over the keys of the hash and keeping track of the character with the maximum count.

3. **Least Frequent Character:** Similarly, it finds the character with the lowest count in the hash.

4. **First Index of Most Frequent Character:** It uses the `first_index()` function from the `List::MoreUtils` module to find the index of the first occurrence of the most frequent character in the input string.

5. **Last Index of Least Frequent Character:** It uses the `rindex()` function to find the index of the last occurrence of the least frequent character in the input string.

6. **Printing the Results:** The code prints the results, including the most and least frequent characters, their counts, and their first and last indices. It also prints the character counts hash using the `Data::Dumper` module for better visualization.

This code provides a comprehensive analysis of the characters in the input string, making it useful for various text processing applications.