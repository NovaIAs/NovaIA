```tcl
# Create two lists of values
list1 = {a b c d e f g h i j k l m n o p q r s t u v w x y z}
list2 = {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20}

# Define a procedure to concatenate two lists
concat {list1 list2} {
    # Create a new list to store the concatenated values
    set newlist {}

    # Loop through the first list and append each value to the new list
    foreach value $list1 {
        lappend newlist $value
    }

    # Loop through the second list and append each value to the new list
    foreach value $list2 {
        lappend newlist $value
    }

    # Return the concatenated list
    return $newlist
}

# Call the procedure to concatenate the two lists
set concatenated [concat $list1 $list2]

# Print the concatenated list
puts $concatenated
```

Explanation:

1. Create two lists of values (`list1` and `list2`) containing letters and numbers, respectively.

2. Define a procedure named `concat` that takes two lists as input and concatenates them into a single list.

3. Inside the `concat` procedure, create a new empty list called `newlist` to store the concatenated values.

4. Use a `foreach` loop to iterate through the first input list (`list1`) and append each value to the `newlist`.

5. Use another `foreach` loop to iterate through the second input list (`list2`) and append each value to the `newlist`.

6. Return the concatenated list (`newlist`) from the procedure.

7. Call the `concat` procedure to concatenate the `list1` and `list2` lists and store the result in the `concatenated` variable.

8. Use the `puts` command to print the concatenated list to the console.

In this code, we've created a procedure called `concat` that can be used to concatenate any two lists, not just the specific `list1` and `list2` lists. This makes the code more reusable and versatile.