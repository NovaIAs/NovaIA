```perl
use strict;
use warnings;

# Define a hash to store the population data
my %population;

# Open the data file
open(my $data_file, '<', 'population.csv') or die "Could not open population data file: $!";

# Read the header line
my $header_line = <$data_file>;

# Split the header line into columns
my @columns = split(/,/, $header_line);

# Read the data lines
while (my $line = <$data_file>) {
    # Split the line into columns
    my @fields = split(/,/, $line);

    # Store the population data in the hash
    $population{$fields[0]} = {
        country  => $fields[1],
        year     => $fields[2],
        population => $fields[3]
    };
}

# Close the data file
close($data_file);

# Prompt the user to enter a country
print "Enter a country: ";
my $country = <STDIN>;
chomp($country);

# Display the population data for the specified country
if (exists($population{$country})) {
    print "Population data for $country:\n";
    foreach my $year (sort keys %{$population{$country}}) {
        print "$year: ", $population{$country}{$year}{'population'}, "\n";
    }
} else {
    print "No population data found for $country.\n";
}
```

Explanation:

1. We start by importing the `strict` and `warnings` modules to enable strict variable declaration and warnings for potential errors.

2. We define a hash called `%population` to store the population data.

3. We open the `population.csv` file in read mode using the `open()` function and check if it was successful.

4. We read the header line and split it into columns using `split(/,/, $header_line)`.

5. We enter a loop to read each data line, split it into fields, and store the population data in the `%population` hash using the country, year, and population fields.

6. After reading all the data lines, we close the data file.

7. We prompt the user to enter a country name.

8. We check if the entered country exists in the `%population` hash.

9. If the country exists, we iterate over the years for that country and display the population data for each year.

10. If the country does not exist, we display a message indicating that no population data was found for the specified country.

This code allows you to enter a country name and see the population data for that country over different years. It demonstrates the use of data structures (hash), file handling, user input handling, and conditional statements.