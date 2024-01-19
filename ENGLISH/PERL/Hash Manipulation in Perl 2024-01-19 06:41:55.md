```perl
use strict;
use warnings;

# Define a hash to store the data
my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'address' => '123 Main Street',
    'city' => 'Anytown',
    'state' => 'CA',
    'zip' => '12345',
    'phone' => '123-456-7890',
    'email' => 'johndoe@example.com',
);

# Define a subroutine to print the data
sub print_data {
    my ($data) = @_;
    foreach my $key (keys %$data) {
        print "$key: $data->{$key}\n";
    }
}

# Print the data
print_data(\%data);

# Define a subroutine to add a new key-value pair to the data
sub add_data {
    my ($data, $key, $value) = @_;
    $data->{$key} = $value;
}

# Add a new key-value pair to the data
add_data(\%data, 'occupation', 'Software Engineer');

# Print the data
print_data(\%data);

# Define a subroutine to remove a key-value pair from the data
sub remove_data {
    my ($data, $key) = @_;
    delete $data->{$key};
}

# Remove a key-value pair from the data
remove_data(\%data, 'occupation');

# Print the data
print_data(\%data);
```

This code is a complex Perl script that demonstrates several different features of the language, including:

* Using a hash to store data
* Defining a subroutine to print the data
* Defining a subroutine to add a new key-value pair to the data
* Defining a subroutine to remove a key-value pair from the data
* Using a loop to iterate over the keys of a hash

The code is well-commented and easy to understand, even for someone who is not familiar with Perl. It is also a good example of how to use Perl to manage data.