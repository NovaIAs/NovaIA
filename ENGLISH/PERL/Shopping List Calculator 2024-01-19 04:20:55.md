```perl
use strict;
use warnings;

# Define a hash to store the shopping list
my %shopping_list = (
    "apples" => 3,
    "bananas" => 5,
    "oranges" => 2,
    "milk" => 1,
    "eggs" => 6,
    "bread" => 1,
    "cheese" => 1,
    "butter" => 1,
);

# Print the shopping list in a formatted table
print "Shopping List:\n";
printf "%-15s %s\n", "Item", "Quantity";
foreach my $item (keys %shopping_list) {
    printf "%-15s %d\n", $item, $shopping_list{$item};
}

# Calculate the total cost of the shopping list
my $total_cost = 0;
foreach my $item (keys %shopping_list) {
    my $price = get_price($item);
    my $quantity = $shopping_list{$item};
    $total_cost += $price * $quantity;
}

# Print the total cost
printf "\nTotal Cost: $%.2f\n", $total_cost;

# Define a subroutine to get the price of an item
sub get_price {
    my $item = shift;

    # Define a hash to store the prices of items
    my %prices = (
        "apples" => 1.99,
        "bananas" => 0.99,
        "oranges" => 1.49,
        "milk" => 2.99,
        "eggs" => 2.49,
        "bread" => 1.99,
        "cheese" => 3.99,
        "butter" => 2.99,
    );

    return $prices{$item};
}
```

Explanation:

1. We start by defining the shopping list as a hash, with the keys being the items and the values being the quantities.


2. We then print the shopping list in a formatted table, using the `printf` function to create a table with two columns: "Item" and "Quantity".


3. Next, we calculate the total cost of the shopping list by iterating over the items in the list and multiplying the price of each item by the quantity. We define a subroutine called `get_price` to get the price of each item.


4. The `get_price` subroutine takes an item as an argument and returns its price by looking it up in a hash containing the prices of all items.


5. Finally, we print the total cost of the shopping list, using the `printf` function to format the output with two decimal places.