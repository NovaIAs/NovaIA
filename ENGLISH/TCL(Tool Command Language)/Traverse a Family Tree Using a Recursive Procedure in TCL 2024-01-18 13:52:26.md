```tcl
# This script creates a complex hierarchical data structure representing a family tree.
# It then uses a recursive procedure to traverse the tree and print out the names of
# all the family members.

# First, define the data structure.  Each family member is represented by a list,
# containing the person's name, their relationship to the person who owns the list,
# and a list of their children.

set family {
    {name John Doe relationship father children {
        {name Jane Doe relationship daughter children {}}
        {name John Doe Jr. relationship son children {}}
    }}
    {name Mary Doe relationship mother children {
        {name Jane Doe relationship daughter children {}}
        {name John Doe Jr. relationship son children {}}
    }}
    {name Grandpa Doe relationship grandfather children {
        {name John Doe relationship son children {
            {name Jane Doe relationship granddaughter children {}}
            {name John Doe Jr. relationship grandson children {}}
        }}
        {name Mary Doe relationship daughter children {
            {name Jane Doe relationship granddaughter children {}}
            {name John Doe Jr. relationship grandson children {}}
        }}
    }}
    {name Grandma Doe relationship grandmother children {
        {name John Doe relationship son children {
            {name Jane Doe relationship granddaughter children {}}
            {name John Doe Jr. relationship grandson children {}}
        }}
        {name Mary Doe relationship daughter children {
            {name Jane Doe relationship granddaughter children {}}
            {name John Doe Jr. relationship grandson children {}}
        }}
    }}
}

# Next, define a recursive procedure to traverse the tree and print out the names of
# all the family members.

proc print_family {family} {
    foreach person $family {
        puts "[list index $person 0] is a [list index $person 1]."
        print_family [list index $person 2]
    }
}

# Finally, call the print_family procedure to print out the names of all the family
# members.

print_family $family
```

This code defines a data structure to represent a family tree, with each family member represented by a list containing their name, their relationship to the person who owns the list, and a list of their children.

It then defines a recursive procedure called `print_family` that takes a list representing a family tree as its argument and prints out the names of all the family members. The procedure uses a foreach loop to iterate over the list of family members and print out their names and relationships. It then calls itself recursively on the list of each family member's children.

Finally, the code calls the `print_family` procedure with the family tree data structure as its argument, which prints out the names of all the family members in the tree.