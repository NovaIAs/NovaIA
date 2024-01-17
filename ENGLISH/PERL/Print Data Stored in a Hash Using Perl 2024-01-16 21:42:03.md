```perl
use strict;
use warnings;

# Define a hash to store the data
my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'occupation' => 'Software Engineer',
    'hobbies' => [
        'reading',
        'writing',
        'hiking',
        'camping'
    ],
    'friends' => [
        {
            'name' => 'Jane Smith',
            'age' => 25,
            'occupation' => 'Teacher'
        },
        {
            'name' => 'Bob Jones',
            'age' => 35,
            'occupation' => 'Doctor'
        }
    ]
);

# Print the data using a loop
foreach my $key (keys %data) {
    my $value = $data{$key};

    if (ref $value eq 'ARRAY') {
        print "$key: @{$value}\n";
    } elsif (ref $value eq 'HASH') {
        print "$key:\n";
        foreach my $subkey (keys %$value) {
            my $subvalue = $value->{$subkey};
            print "  $subkey: $subvalue\n";
        }
    } else {
        print "$key: $value\n";
    }
}
```

This code is a Perl script that defines a hash to store data about a person, including their name, age, occupation, hobbies, and friends. The script then uses a loop to print the data in a human-readable format.

The hash is defined using the %data variable. The keys of the hash are the names of the data items, and the values are the data items themselves. In this case, the data items are strings, arrays, and hashes.

The loop uses the keys() function to get a list of the keys in the hash. The foreach loop then iterates over the list of keys, printing the key and the value of the hash element.

If the value of the hash element is an array, the @{} operator is used to print the elements of the array. If the value of the hash element is a hash, the %{} operator is used to print the keys and values of the hash. Otherwise, the value of the hash element is printed as a string.

This code is complex because it uses a number of different data structures and control structures. However, it is also well-organized and easy to read. The comments in the code explain what each part of the code does, making it easy to understand how the code works.