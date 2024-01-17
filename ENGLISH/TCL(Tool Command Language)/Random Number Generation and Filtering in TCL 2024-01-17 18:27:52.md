```tcl
proc init {options} {
    # Parse command-line options
    set init_options [getopts -long interval:,max:,min:, -- interval max min]
    if {$init_options(invalid) ne ""} {
        puts "Invalid option: $init_options(invalid)"
        return -code error
    }

    # Set default values for missing options
    set interval [lindex $init_options(interval) 0 10]
    set max [lindex $init_options(max) 0 100]
    set min [lindex $init_options(min) 0 0]

    # Check if values are valid
    if {$interval < 1} {
        puts "Interval must be greater than 0"
        return -code error
    }
    if {$max < $min} {
        puts "Maximum value must be greater than or equal to minimum value"
        return -code error
    }

    # Initialize random number generator
    srand [clock milliseconds]

    # Create list of random numbers
    set random_numbers {}
    for {set i 0} {$i < $max} {incr i $interval} {
        lappend random_numbers [rand $min $max]
    }

    # Return list of random numbers
    return $random_numbers
}

proc filter {random_numbers threshold} {
    # Filter random numbers based on threshold
    set filtered_numbers {}
    foreach number $random_numbers {
        if {$number > $threshold} {
            lappend filtered_numbers $number
        }
    }

    # Return list of filtered numbers
    return $filtered_numbers
}

proc main {args} {
    # Parse command-line arguments
    if {[llength $args] != 3} {
        puts "Usage: tclsh script.tcl interval max min"
        return -code error
    }
    set interval [lindex $args 0]
    set max [lindex $args 1]
    set min [lindex $args 2]

    # Initialize random number generator and create list of random numbers
    set random_numbers [init -interval $interval -max $max -min $min]

    # Filter random numbers
    set threshold [expr {$max - $min} / 2]
    set filtered_numbers [filter $random_numbers $threshold]

    # Print results
    puts "Random numbers:"
    foreach number $random_numbers {
        puts "    $number"
    }
    puts "Filtered numbers:"
    foreach number $filtered_numbers {
        puts "    $number"
    }
}

# Run main function
main [lrange $argv 1 end]
```

Explanation:

- The `init` procedure initializes the random number generator and creates a list of random numbers within a specified range and interval. It takes optional command-line arguments for the interval, maximum value, and minimum value. If any of the arguments are missing or invalid, it displays an error message and returns an error code.


- The `filter` procedure filters a list of random numbers based on a specified threshold. It returns a new list containing only the numbers that are greater than the threshold.


- The `main` procedure parses the command-line arguments and calls the `init` procedure to generate a list of random numbers. It then calls the `filter` procedure to filter the numbers based on a calculated threshold. Finally, it prints the original list of random numbers and the filtered list of numbers.


- The script is executed by calling the `main` procedure with the command-line arguments. It then displays the results on the console.