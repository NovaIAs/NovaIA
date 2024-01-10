```perl
#!/usr/bin/perl

use strict;
use warnings;

my $input_file = 'input.txt';
my $output_file = 'output.txt';

# Open the input file for reading
open(my $fh, '<', $input_file) or die "Could not open input file: $!";

# Create an empty array to store the lines of the input file
my @lines = ();

# Read each line of the input file and add it to the array
while (my $line = <$fh>) {
    chomp $line;
    push @lines, $line;
}

# Close the input file
close($fh);

# Create an empty array to store the unique lines of the input file
my @unique_lines = ();

# Iterate over the lines of the input file and add each unique line to the array
foreach my $line (@lines) {
    if (!grep { $_ eq $line } @unique_lines) {
        push @unique_lines, $line;
    }
}

# Open the output file for writing
open(my $fh, '>', $output_file) or die "Could not open output file: $!";

# Write each unique line of the input file to the output file
foreach my $line (@unique_lines) {
    print $fh "$line\n";
}

# Close the output file
close($fh);

# Print a message to the console indicating that the unique lines have been written to the output file
print "Unique lines written to $output_file\n";
```

This Perl script reads a text file, removes duplicate lines, and writes the unique lines to a new text file.

The script first opens the input file for reading and reads each line of the file into an array. It then closes the input file.

Next, the script creates an empty array to store the unique lines of the input file. It then iterates over the lines of the input file and adds each unique line to the array.

To determine if a line is unique, the script uses the `grep` function to check if the line already exists in the array of unique lines. If the line does not exist in the array, it is added to the array.

Once all of the unique lines have been added to the array, the script opens the output file for writing and writes each unique line to the file. It then closes the output file.

Finally, the script prints a message to the console indicating that the unique lines have been written to the output file.

This script is a good example of how to use Perl to read a file, process the data in the file, and write the results to a new file.