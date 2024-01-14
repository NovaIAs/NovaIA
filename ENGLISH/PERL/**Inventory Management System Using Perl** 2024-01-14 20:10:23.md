```perl
use strict;
use warnings;

# Declare a hash to store the inventory
my %inventory;

# Add some items to the inventory
$inventory{'apples'} = 10;
$inventory{'oranges'} = 5;
$inventory{'bananas'} = 15;

# Print the inventory
print "Inventory:\n";
foreach my $item (keys %inventory) {
    print "$item: $inventory{$item}\n";
}

# Create a subroutine to add items to the inventory
sub add_to_inventory {
    my ($item, $quantity) = @_;
    $inventory{$item} += $quantity;
}

# Add some more items to the inventory using the subroutine
add_to_inventory('apples', 5);
add_to_inventory('oranges', 10);

# Print the updated inventory
print "\nUpdated Inventory:\n";
foreach my $item (keys %inventory) {
    print "$item: $inventory{$item}\n";
}

# Create a subroutine to remove items from the inventory
sub remove_from_inventory {
    my ($item, $quantity) = @_;
    $inventory{$item} -= $quantity;
    if ($inventory{$item} <= 0) {
        delete $inventory{$item};
    }
}

# Remove some items from the inventory using the subroutine
remove_from_inventory('apples', 3);
remove_from_inventory('oranges', 7);

# Print the updated inventory
print "\nUpdated Inventory:\n";
foreach my $item (keys %inventory) {
    print "$item: $inventory{$item}\n";
}

# Create a subroutine to check if an item is in the inventory
sub is_in_inventory {
    my ($item) = @_;
    return (exists $inventory{$item});
}

# Check if an item is in the inventory using the subroutine
my $item_to_check = 'pears';
if (is_in_inventory($item_to_check)) {
    print "\n$item_to_check is in the inventory.\n";
} else {
    print "\n$item_to_check is not in the inventory.\n";
}
```

**Explanation:**

This Perl code demonstrates a more complex and differentiated approach to managing an inventory using a hash and various subroutines. Here's how each part of the code works:

1. **Inventory Hash:**
   - A hash named `%inventory` is created to store the inventory items and their quantities.

2. **Adding Items:**
   - Items are added to the inventory using direct assignment to the hash. For example, `$inventory{'apples'} = 10;` adds 10 apples to the inventory.

3. **Printing the Inventory:**
   - A loop is used to iterate through the keys of the `%inventory` hash and print the item names and their quantities.

4. **Subroutine for Adding Items:**
   - A subroutine called `add_to_inventory()` is defined. It takes two parameters: the item to be added and the quantity. It increments the quantity of the specified item in the inventory.

5. **Adding Items Using Subroutine:**
   - The `add_to_inventory()` subroutine is used to add more items to the inventory.

6. **Subroutine for Removing Items:**
   - A subroutine called `remove_from_inventory()` is defined. It takes two parameters: the item to be removed and the quantity. It decrements the quantity of the specified item in the inventory and removes it if the quantity becomes zero or negative.

7. **Removing Items Using Subroutine:**
   - The `remove_from_inventory()` subroutine is used to remove some items from the inventory.

8. **Subroutine for Checking Item Availability:**
   - A subroutine called `is_in_inventory()` is defined. It takes an item name as a parameter and checks if the item exists in the inventory. It returns `true` if the item is present; otherwise, it returns `false`.

9. **Checking Item Availability:**
   - The `is_in_inventory()` subroutine is used to check if a specific item is in the inventory.

This code provides a more robust and modular way to manage an inventory, allowing for easy addition, removal, and checking of items. It demonstrates the use of subroutines, control structures, and data structures in Perl.