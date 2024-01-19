```tcl
# Complex TCL Code

# Define a procedure to calculate the factorial of a number
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr $n * [factorial [expr $n - 1]]]
    }
}

# Define a list of numbers
set numbers {1 2 3 4 5 6 7 8 9 10}

# Create a new list to store the factorials of the numbers
set factorials {}

# Loop through the list of numbers and calculate the factorial of each number
foreach number $numbers {
    set factorial [factorial $number]

    # Add the factorial to the list of factorials
    lappend factorials $factorial
}

# Print the list of numbers and their corresponding factorials
foreach {number factorial} $numbers $factorials {
    puts "$number: $factorial"
}

# Reverse the list of factorials
set reversed_factorials [lreverse $factorials]

# Print the reversed list of factorials
foreach factorial $reversed_factorials {
    puts $factorial
}

# Sort the list of numbers in descending order
set sorted_numbers [lsort -integer -decreasing $numbers]

# Print the sorted list of numbers
foreach number $sorted_numbers {
    puts $number
}

# Find the maximum value in the list of numbers
set max_number [lindex $sorted_numbers 0]

# Print the maximum value
puts "Maximum value: $max_number"

# Find the minimum value in the list of numbers
set min_number [lindex $sorted_numbers end]

# Print the minimum value
puts "Minimum value: $min_number"

```

Explanation:

1. We define a procedure called "factorial" to calculate the factorial of a number. The factorial of a number is the product of all the positive integers up to that number. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.
2. We create a list of numbers called "numbers".
3. We create a new list called "factorials" to store the factorials of the numbers.
4. We loop through the list of numbers and calculate the factorial of each number using the "factorial" procedure. We then add the factorial to the list of factorials.
5. We print the list of numbers and their corresponding factorials.
6. We reverse the list of factorials using the "lreverse" command.
7. We print the reversed list of factorials.
8. We sort the list of numbers in descending order using the "lsort" command with the "-integer" and "-decreasing" options.
9. We print the sorted list of numbers.
10. We find the maximum value in the list of numbers using the "lindex" command with the "0" index.
11. We print the maximum value.
12. We find the minimum value in the list of numbers using the "lindex" command with the "end" index.
13. We print the minimum value.