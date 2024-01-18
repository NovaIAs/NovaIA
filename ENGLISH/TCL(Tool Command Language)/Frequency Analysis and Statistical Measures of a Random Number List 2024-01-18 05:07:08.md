```tcl
proc makeComplexCode {} {
    # Create a list of random numbers
    set myList [lrange [lsort -integer -decreasing [genrand 1000]]] 0 9]

    # Create a dictionary to store the frequency of each number
    set freq {}
    foreach num $myList {
        incr freq($num)
    }

    # Sort the dictionary by the frequency of the numbers
    set sortedFreq [lsort -integer -decreasing [lreverse [lmap k v [dict keys $freq] [dict values $freq]]]]

    # Print the top 10 most frequent numbers and their frequencies
    puts "Top 10 Most Frequent Numbers and Their Frequencies:"
    foreach {num freq} [lrange $sortedFreq 0 9] {
        puts "  $num: $freq"
    }

    # Calculate the mean, median, and mode of the list
    set mean [expr {[llength $myList] / 2}]
    set median [lindex $myList $mean]
    set mode [lindex $sortedFreq 0]

    # Print the mean, median, and mode of the list
    puts "Mean: $mean"
    puts "Median: $median"
    puts "Mode: $mode"
}

# Call the makeComplexCode procedure
makeComplexCode

```

Explanation:

1. The makeComplexCode procedure is defined, which takes no arguments.

2. A list of 1000 random integers is generated using the genrand command and stored in the myList variable.

3. A dictionary called freq is created to store the frequency of each number in the myList. The foreach loop iterates through the myList and increments the frequency of each number in the freq dictionary.

4. The sortedFreq variable is created by sorting the freq dictionary by the frequency of the numbers in descending order using the lsort command.

5. The top 10 most frequent numbers and their frequencies are printed using the puts command and a foreach loop.

6. The mean, median, and mode of the myList are calculated using the expr, lindex, and llength commands.

7. The mean, median, and mode are printed using the puts command.

8. The makeComplexCode procedure is called to execute the code.