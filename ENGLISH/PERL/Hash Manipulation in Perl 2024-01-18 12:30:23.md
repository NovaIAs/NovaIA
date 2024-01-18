```perl
use strict;
use warnings;

# Define a hash to store the data
my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'city' => 'New York',
    'state' => 'NY',
    'zip' => '10001',
    'phone' => '212-555-1212',
    'email' => 'johndoe@example.com',
);

# Define a subroutine to print the data
sub print_data {
    my $data = shift;

    foreach my $key (keys %$data) {
        print "$key: $data->{$key}\n";
    }
}

# Print the data
print_data(\%data);

# Define a subroutine to add a new item to the hash
sub add_item {
    my $data = shift;
    my $key = shift;
    my $value = shift;

    $data->{$key} = $value;
}

# Add a new item to the hash
add_item(\%data, 'favorite_color', 'blue');

# Print the data again
print_data(\%data);

# Define a subroutine to delete an item from the hash
sub delete_item {
    my $data = shift;
    my $key = shift;

    delete $data->{$key};
}

# Delete an item from the hash
delete_item(\%data, 'zip');

# Print the data again
print_data(\%data);

# Define a subroutine to find a value in the hash
sub find_value {
    my $data = shift;
    my $key = shift;

    return $data->{$key};
}

# Find a value in the hash
my $value = find_value(\%data, 'name');

# Print the value
print "Name: $value\n";

# Define a subroutine to check if a key exists in the hash
sub key_exists {
    my $data = shift;
    my $key = shift;

    return exists $data->{$key};
}

# Check if a key exists in the hash
my $exists = key_exists(\%data, 'favorite_color');

# Print the result
print "Favorite color exists: $exists\n";
```

This code is a Perl script that demonstrates various operations on a hash. It defines a hash to store data, and then defines a series of subroutines to perform operations on the hash. The subroutines are used to print the data, add a new item to the hash, delete an item from the hash, find a value in the hash, and check if a key exists in the hash. The script also includes comments to explain the code.