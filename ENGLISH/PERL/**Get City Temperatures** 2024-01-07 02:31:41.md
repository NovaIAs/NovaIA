```perl
use strict;
use warnings;

# Define a hash to store the temperature data
my %temperatures;

# Define an array to store the cities
my @cities = ('London', 'Paris', 'Rome', 'Tokyo', 'New York');

# Get the current temperature for each city and store it in the hash
foreach my $city (@cities) {
    my $temperature = get_temperature($city);
    $temperatures{$city} = $temperature;
}

# Print the temperature for each city
foreach my $city (@cities) {
    print "$city: $temperatures{$city}\n";
}

# Get the temperature for a given city
sub get_temperature {
    my $city = shift;

    # Simulate getting the temperature from a weather API
    my $temperature = int(rand(100));

    return $temperature;
}
```

Explanation:

1. We start by importing the `strict` and `warnings` modules to enable strict variable declarations and runtime warnings, respectively.

2. We define a hash called `%temperatures` to store the temperature data for each city.

3. We define an array called `@cities` to store the list of cities for which we want to get the temperature.

4. We use a `foreach` loop to iterate through each city in the `@cities` array.

5. Inside the loop, we call the `get_temperature()` subroutine to get the current temperature for the current city and store it in the `%temperatures` hash.

6. After getting the temperature for all cities, we use another `foreach` loop to iterate through the cities again and print the temperature for each city.

7. Finally, we define the `get_temperature()` subroutine, which takes a city name as an argument. It simulates getting the temperature from a weather API by generating a random integer between 0 and 99 (inclusive) and returning it as the temperature.