```perl
use strict;
use warnings;

# Define a subroutine to parse a line of input
sub parse_line {
    my $line = shift;
    my @fields = split /\t/, $line;
    return @fields;
}

# Define a subroutine to process a line of input
sub process_line {
    my @fields = shift;
    my $name = $fields[0];
    my $age = $fields[1];
    my $gender = $fields[2];
    my $city = $fields[3];

    # Print the information about the person
    print "Name: $name\n";
    print "Age: $age\n";
    print "Gender: $gender\n";
    print "City: $city\n";
}

# Open the input file
open(my $fh, '<', 'input.txt') or die "Could not open input file: $!";

# Read each line of the input file
while (my $line = <$fh>) {
    # Parse the line of input
    my @fields = parse_line($line);

    # Process the line of input
    process_line(@fields);
}

# Close the input file
close($fh);
```

This code is a Perl script that reads a text file containing information about people and prints the information about each person to the console.

The code first defines two subroutines: `parse_line()` and `process_line()`. The `parse_line()` subroutine takes a line of input and splits it into an array of fields, based on the tab character. The `process_line()` subroutine takes an array of fields and prints the information about the person to the console.

The code then opens the input file and reads each line of the file. For each line of the file, the code calls the `parse_line()` subroutine to parse the line and the `process_line()` subroutine to process the line.

Finally, the code closes the input file.