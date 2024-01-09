```perl
use strict;
use warnings;

# Define a complex data structure to hold information about a person.
my %person = (
    name => 'John Doe',
    age => 30,
    address => {
        street => '123 Main Street',
        city => 'Anytown',
        state => 'CA',
        zip => '12345'
    },
    phone => {
        home => '555-1212',
        work => '555-2323',
        cell => '555-3434'
    },
    email => 'johndoe@example.com'
);

# Iterate over the person's data structure and print out each piece of information.
foreach my $key (keys %person) {
    my $value = $person{$key};

    if (ref($value) eq 'HASH') {
        # If the value is a hash, iterate over its keys and print out each piece of information.
        foreach my $subkey (keys %$value) {
            my $subvalue = $value->{$subkey};
            print "$subkey: $subvalue\n";
        }
    }
    else {
        # If the value is not a hash, just print it out.
        print "$key: $value\n";
    }
}

# Print out a separator line.
print "----------------------------------------\n";

# Use a regular expression to extract the area code from the person's home phone number.
my $area_code = $person{'phone'}{'home'} =~ s/(\d{3})-(\d{4})/$1/;

# Print out the area code.
print "Area code: $area_code\n";
```

Explanation:

* The first few lines of the code use the `use strict` and `use warnings` pragmas to enforce strict syntax and warn about potential problems in the code.
* The next line defines a complex data structure to hold information about a person. The data structure is a hash, which is a collection of key-value pairs.
* The next line iterates over the person's data structure and prints out each piece of information.
* If the value of a key is a hash, the code iterates over the keys of the hash and prints out each piece of information.
* If the value of a key is not a hash, the code just prints it out.
* The next line prints out a separator line.
* The next few lines of code use a regular expression to extract the area code from the person's home phone number.
* The last line of the code prints out the area code.