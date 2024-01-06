```perl
use strict;
use warnings;

# Define a hash to store the names and ages of people.
my %people = (
    'Alice' => 21,
    'Bob'   => 25,
    'Carol' => 30,
    'Dave'  => 35,
    'Eve'   => 40,
);

# Define a subroutine to print the names and ages of people.
sub print_people {
    my $name;
    my $age;

    # Iterate over the keys of the %people hash.
    foreach $name (keys %people) {
        # Get the age of the person with the current name.
        $age = $people{$name};

        # Print the name and age of the person.
        print "$name is $age years old.\n";
    }
}

# Define a subroutine to add a new person to the %people hash.
sub add_person {
    my $name = shift;
    my $age = shift;

    # Add the new person to the %people hash.
    $people{$name} = $age;

    # Print a message indicating that the new person has been added.
    print "$name has been added to the list of people.\n";
}

# Define a subroutine to delete a person from the %people hash.
sub delete_person {
    my $name = shift;

    # Delete the person from the %people hash.
    delete $people{$name};

    # Print a message indicating that the person has been deleted.
    print "$name has been deleted from the list of people.\n";
}

# Print the names and ages of people.
print_people();

# Add a new person to the %people hash.
add_person('Frank', 45);

# Print the names and ages of people.
print_people();

# Delete a person from the %people hash.
delete_person('Bob');

# Print the names and ages of people.
print_people();
```

This code defines three subroutines: `print_people()`, `add_person()`, and `delete_person()`. The `print_people()` subroutine prints the names and ages of people stored in the `%people` hash. The `add_person()` subroutine adds a new person to the `%people` hash. The `delete_person()` subroutine deletes a person from the `%people` hash.

The main part of the code uses the `print_people()`, `add_person()`, and `delete_person()` subroutines to demonstrate how to use them. First, the `print_people()` subroutine is called to print the names and ages of people stored in the `%people` hash. Then, the `add_person()` subroutine is called to add a new person to the `%people` hash. The `print_people()` subroutine is called again to print the names and ages of people stored in the `%people` hash. Finally, the `delete_person()` subroutine is called to delete a person from the `%people` hash. The `print_people()` subroutine is called again to print the names and ages of people stored in the `%people` hash.