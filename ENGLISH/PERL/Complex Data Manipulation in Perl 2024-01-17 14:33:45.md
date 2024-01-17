```perl
use strict;
use warnings;
use Data::Dumper;

my $data = {
    name => 'John Doe',
    age => 30,
    address => {
        street => '123 Main Street',
        city => 'Anytown',
        state => 'CA',
        zip => '12345',
    },
    phone => [
        {
            type => 'home',
            number => '555-1212',
        },
        {
            type => 'work',
            number => '555-2323',
        },
    ],
};

# Print the data structure
print Dumper($data);

# Access a specific value
my $name = $data->{name};
print "Name: $name\n";

# Iterate over the phone numbers
foreach my $phone (@{$data->{phone}}) {
    print "Phone: $phone->{type} - $phone->{number}\n";
}

# Add a new phone number
push @{$data->{phone}}, {
    type => 'mobile',
    number => '555-3434',
};

# Print the updated data structure
print Dumper($data);
```

This code creates a complex data structure in Perl, and then performs various operations on it, such as printing the structure, accessing a specific value, iterating over a list of values, and adding a new value to the list. The code is explained in detail below:

```
use strict;
use warnings;
use Data::Dumper;
```

These lines import the strict, warnings, and Data::Dumper modules. The strict module enforces strict variable declaration and use, while the warnings module enables warnings for potential problems in the code. The Data::Dumper module is used to print the data structure in a human-readable format.

```
my $data = {
    name => 'John Doe',
    age => 30,
    address => {
        street => '123 Main Street',
        city => 'Anytown',
        state => 'CA',
        zip => '12345',
    },
    phone => [
        {
            type => 'home',
            number => '555-1212',
        },
        {
            type => 'work',
            number => '555-2323',
        },
    ],
};
```

This code creates a complex data structure, which is a hash of hashes and arrays. The keys of the hash are 'name', 'age', 'address', and 'phone'. The value of the 'address' key is another hash, which contains the keys 'street', 'city', 'state', and 'zip'. The value of the 'phone' key is an array of hashes, each of which contains the keys 'type' and 'number'.

```
# Print the data structure
print Dumper($data);
```

This line prints the data structure using the Data::Dumper module. This allows you to see the structure of the data in a human-readable format.

```
# Access a specific value
my $name = $data->{name};
print "Name: $name\n";
```

This code accesses the 'name' value from the data structure and prints it out.

```
# Iterate over the phone numbers
foreach my $phone (@{$data->{phone}}) {
    print "Phone: $phone->{type} - $phone->{number}\n";
}
```

This code iterates over the 'phone' array of hashes and prints out the 'type' and 'number' values for each hash.

```
# Add a new phone number
push @{$data->{phone}}, {
    type => 'mobile',
    number => '555-3434',
};
```

This code adds a new hash to the 'phone' array. The hash contains the keys 'type' and 'number', and the values 'mobile' and '555-3434', respectively.

```
# Print the updated data structure
print Dumper($data);
```

This line prints the updated data structure, which now includes the new phone number.