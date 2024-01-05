```tcl
# Define a procedure to generate random numbers within a specified range.
proc generate_random_number {min max} {
    expr {int(rand() * ($max - $min + 1)) + $min}
}

# Define a procedure to create a list of random numbers.
proc create_random_list {length min max} {
    set random_list {}
    for {set i 0} {$i < $length} {incr i} {
        lappend random_list [generate_random_number $min $max]
    }
    return $random_list
}

# Define a procedure to calculate the average of a list of numbers.
proc calculate_average {numbers} {
    set sum 0
    foreach number $numbers {
        set sum [expr {$sum + $number}]
    }
    return [expr {$sum / [llength $numbers]}]
}

# Define a procedure to calculate the standard deviation of a list of numbers.
proc calculate_standard_deviation {numbers} {
    set average [calculate_average $numbers]
    set sum_of_squared_differences 0
    foreach number $numbers {
        set difference [expr {$number - $average}]
        set sum_of_squared_differences [expr {$sum_of_squared_differences + ($difference * $difference)}]
    }
    return [expr {sqrt([expr {$sum_of_squared_differences / [llength $numbers]}])}]
}

# Generate a list of 100 random numbers between 1 and 100.
set random_list [create_random_list 100 1 100]

# Calculate the average and standard deviation of the list.
set average [calculate_average $random_list]
set standard_deviation [calculate_standard_deviation $random_list]

# Print the results.
puts "Random List: $random_list"
puts "Average: $average"
puts "Standard Deviation: $standard_deviation"
```

Explanation:

1. **Procedure Definitions**: Three procedures are defined:

   - `generate_random_number`: Generates a random number within a specified range.
   - `create_random_list`: Creates a list of random numbers of a specified length and range.
   - `calculate_average`: Calculates the average of a list of numbers.
   - `calculate_standard_deviation`: Calculates the standard deviation of a list of numbers.

2. **Generate Random List**: A list of 100 random numbers between 1 and 100 is generated using the `create_random_list` procedure and stored in the `random_list` variable.

3. **Calculate Average and Standard Deviation**: The `calculate_average` and `calculate_standard_deviation` procedures are used to calculate the average and standard deviation of the random list, respectively.

4. **Print Results**: The calculated average and standard deviation are printed to the console.

This code demonstrates the use of procedures, loops, mathematical expressions, and list manipulation in Tcl. It provides a clear and concise way to generate random numbers, calculate statistical measures, and display the results.