```perl
use strict;
use warnings;

# Define a hash to store the inventory
my %inventory = (
    "apples" => 10,
    "oranges" => 5,
    "bananas" => 3,
    "grapes" => 2,
    "kiwi" => 1
);

# Print the inventory
print "Inventory:\n";
foreach my $fruit (keys %inventory) {
    print "$fruit: $inventory{$fruit}\n";
}

# Get the user's input
print "What fruit would you like to buy? ";
my $fruit = <STDIN>;
chomp $fruit;

# Check if the fruit is in the inventory
if (exists $inventory{$fruit}) {
    # Get the quantity of fruit the user wants to buy
    print "How many $fruit would you like to buy? ";
    my $quantity = <STDIN>;
    chomp $quantity;

    # Check if there is enough fruit in stock
    if ($quantity <= $inventory{$fruit}) {
        # Update the inventory
        $inventory{$fruit} -= $quantity;

        # Print a confirmation message
        print "You have purchased $quantity $fruit.\n";
    } else {
        # Print an error message
        print "Sorry, there is not enough $fruit in stock.\n";
    }
} else {
    # Print an error message
    print "Sorry, we do not have $fruit in stock.\n";
}

# Print the updated inventory
print "\nUpdated Inventory:\n";
foreach my $fruit (keys %inventory) {
    print "$fruit: $inventory{$fruit}\n";
}
```

This Perl script simulates a simple inventory system. It starts by defining a hash (%inventory) to store the inventory, with each key being a fruit and each value being the quantity of that fruit in stock.

Next, it prints the initial inventory to the console.

Then, it prompts the user to enter the name of a fruit they would like to buy and the quantity they want to purchase.

The script checks if the fruit is in the inventory and if there is enough stock to fulfill the order. If both conditions are met, it updates the inventory and prints a confirmation message. Otherwise, it prints an error message.

Finally, the script prints the updated inventory to the console.