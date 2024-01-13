```perl
use strict;
use warnings;
use Data::Dumper;

my %data = (
    'name' => 'John Doe',
    'age' => 30,
    'address' => {
        'street' => '123 Main Street',
        'city' => 'Anytown',
        'state' => 'CA',
        'zip' => '12345',
    },
    'hobbies' => [
        'reading',
        'hiking',
        'fishing',
    ],
);

# Print the data using Data::Dumper
print Dumper(\%data);

# Access the data using the hash syntax
print "Name: $data{'name'}\n";
print "Age: $data{'age'}\n";
print "Street: $data{'address'}{'street'}\n";
print "City: $data{'address'}{'city'}\n";
print "State: $data{'address'}{'state'}\n";
print "Zip: $data{'address'}{'zip'}\n";

# Loop through the hobbies array
print "Hobbies:\n";
foreach my $hobby (@{$data{'hobbies'}}) {
    print "$hobby\n";
}

# Add a new hobby to the array
push @{$data{'hobbies'}}, 'biking';

# Print the updated data
print Dumper(\%data);
```

Explanation:

1. We start by loading the necessary Perl modules:

   - `use strict;` and `use warnings;` enable strict error checking and warnings, which help us catch errors early.
   - `use Data::Dumper;` loads the Data::Dumper module, which we'll use to print the data structure in a human-readable format.

2. We define a hash (`%data`) to store our data. The hash contains the following keys:

   - `'name'` (value: 'John Doe')
   - `'age'` (value: 30)
   - `'address'` (value: a nested hash with keys `'street'`, `'city'`, `'state'`, and `'zip'`)
   - `'hobbies'` (value: an array of hobbies)

3. We use the `Dumper` function from the Data::Dumper module to print the data structure in a human-readable format.

4. We access the data using the hash syntax (`$data{'name'}`) and print the values of `'name'`, `'age'`, `'address'->{street}'`, `'address'->{city}'`, `'address'->{state}'`, and `'address'->{zip}'`.

5. We loop through the `'hobbies'` array using a `foreach` loop and print each hobby.

6. We add a new hobby ('biking') to the `'hobbies'` array using the `push` function.

7. Finally, we print the updated data structure using the `Dumper` function again.