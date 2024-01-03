```tcl
proc calculatePi {n} {
    # Initialize variables
    set pi 0.0
    set sign 1
    set factorial 1

    # Calculate pi using the Leibnitz formula
    for {set i 0} {$i < $n} {incr i} {
        # Calculate the term for the current iteration
        set term ($sign * (4 / factorial))

        # Add the term to pi
        set pi [expr {$pi + $term}]

        # Update the sign and factorial for the next iteration
        set sign [expr {$sign * -1}]
        set factorial [expr {$factorial * ($i + 1) * ($i + 2)}]
    }

    # Return the calculated value of pi
    return $pi
}

# Calculate pi to 10000 decimal places
set pi [calculatePi 10000]

# Print the first 100 decimal places of pi
puts [format "%.100f\n" $pi]
```

Explanation:

1. `proc calculatePi {n}`: This line defines a Tcl procedure named `calculatePi` that takes one argument, `n`, which represents the number of iterations to use in the calculation.

2. Inside the procedure, several variables are initialized:
   - `pi`: This variable will store the calculated value of pi.
   - `sign`: This variable keeps track of the sign of the term in the Leibnitz formula. It is initially set to 1.
   - `factorial`: This variable stores the factorial of the current term in the Leibnitz formula. It is initially set to 1.

3. The `for` loop iterates `n` times to calculate the terms of the Leibnitz formula:
   - `set term ($sign * (4 / factorial))`: This line calculates the value of the current term using the Leibnitz formula. The `$sign` variable determines the sign of the term, and `4 / factorial` calculates the value of the term.
   - `set pi [expr {$pi + $term}]`: This line adds the current term to the running total in the `pi` variable.
   - `set sign [expr {$sign * -1}]`: This line updates the `sign` variable for the next iteration. It changes the sign from positive to negative or vice versa.
   - `set factorial [expr {$factorial * ($i + 1) * ($i + 2)}]`: This line updates the `factorial` variable for the next iteration. It calculates the factorial of the next term in the Leibnitz formula.

4. After the loop completes, the `return $pi` statement returns the calculated value of pi.

5. The main program calls the `calculatePi` procedure with an argument of 10000 to calculate pi to 10000 decimal places.

6. Finally, the `puts` statement prints the first 100 decimal places of the calculated pi value.

This code uses the Leibnitz formula to calculate pi using a loop. It demonstrates the use of variables, arithmetic operations, loops, and formatting in Tcl.