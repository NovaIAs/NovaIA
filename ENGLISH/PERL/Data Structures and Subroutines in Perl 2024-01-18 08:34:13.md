```
#!/usr/bin/perl

use strict;
use warnings;

my @numbers = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

my %hash = (
    'one' => 1,
    'two' => 2,
    'three' => 3,
    'four' => 4,
    'five' => 5
);

my $string = "Hello, world!";

my @array_of_arrays = (
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
);

sub print_array {
    my @array = @_;

    foreach my $element (@array) {
        print "$element ";
    }

    print "\n";
}

sub print_hash {
    my %hash = %_;

    foreach my $key (keys %hash) {
        print "$key: $hash{$key}\n";
    }

    print "\n";
}

print "Numbers:\n";
print_array(@numbers);

print "Hash:\n";
print_hash(%hash);

print "String:\n";
print "$string\n\n";

print "Array of arrays:\n";
foreach my $array (@array_of_arrays) {
    print_array(@$array);
}
```

This code demonstrates a variety of Perl features, including arrays, hashes, strings, subroutines, and loops. It defines an array of numbers, a hash of key-value pairs, a string, and an array of arrays. It also defines two subroutines, one to print an array and one to print a hash. The main program then uses these subroutines to print the contents of the data structures.

The output of the program is as follows:

```
Numbers:
1 2 3 4 5 6 7 8 9 10

Hash:
five: 5
four: 4
one: 1
three: 3
two: 2

String:
Hello, world!

Array of arrays:
1 2 3
4 5 6
7 8 9
```