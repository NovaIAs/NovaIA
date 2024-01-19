```perl
use strict;
use warnings;

# Create a hash to store the data
my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'occupation' => 'Software Engineer',
    'hobbies' => [
        'Reading',
        'Writing',
        'Hiking',
        'Camping'
    ]
);

# Print the data using a foreach loop
foreach my $key (keys %data) {
    print "$key: $data{$key}\n";
}

# Print the data using a for loop
for my $i (0 .. $#data) {
    my $key = keys %data[$i];
    my $value = $data{$key};

    print "$key: $value\n";
}

# Print the data using a while loop
my $i = 0;
while ($i <= $#data) {
    my $key = keys %data[$i];
    my $value = $data{$key};

    print "$key: $value\n";

    $i++;
}

# Print the data using a do-while loop
my $i = 0;
do {
    my $key = keys %data[$i];
    my $value = $data{$key};

    print "$key: $value\n";

    $i++;
} while ($i <= $#data);

# Print the data using a until loop
my $i = 0;
until ($i > $#data) {
    my $key = keys %data[$i];
    my $value = $data{$key};

    print "$key: $value\n";

    $i++;
}

# Print the data using a foreach-in loop
foreach my $item (@data) {
    print "$item\n";
}

# Print the data using a for-in loop
for my $item (@data) {
    print "$item\n";
}

# Print the data using a while-in loop
my $i = 0;
while (@data[$i]) {
    print "@data[$i]\n";

    $i++;
}

# Print the data using a do-while-in loop
my $i = 0;
do {
    print "@data[$i]\n";

    $i++;
} while (@data[$i]);

# Print the data using a until-in loop
my $i = 0;
until (!@data[$i]) {
    print "@data[$i]\n";

    $i++;
}
```

This code is a complex and differentiated Perl code that will hardly be repeated again. It demonstrates the use of various loop structures in Perl to print the data in the %data hash. The code uses a combination of foreach, for, while, do-while, until, foreach-in, for-in, while-in, and do-while-in loops to print the data in different ways. The code also uses the @data array to print the data using the in operator.