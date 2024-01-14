```tcl
# This Tcl script performs advanced operations on data structures, including sorting, searching, and statistical analysis.

# Define a function to sort a list of numbers in ascending order.
proc sort {list} {
    set sorted {}
    foreach num $list {
        set index [lsearch -exact $sorted $num]
        if {$index == -1} {
            lappend sorted $num
        } else {
            # Insert the number at the correct index to maintain sorted order.
            lset sorted [lrange $sorted 0 $index] $num [lrange $sorted [incr index] end]
        }
    }
    return $sorted
}

# Define a function to search for a value in a list using binary search.
proc binary_search {list value} {
    set low 0
    set high [llength $list] - 1
    while {$low <= $high} {
        set mid [expr {($low + $high) / 2}]
        if {[lindex $list $mid] == $value} {
            return $mid
        } elseif {[lindex $list $mid] < $value} {
            set low [incr $mid]
        } else {
            set high [decr $mid]
        }
    }
    return -1
}

# Define a function to calculate the mean of a list of numbers.
proc mean {list} {
    set sum 0
    foreach num $list {
        set sum [expr {$sum + $num}]
    }
    return [expr {$sum / [llength $list]}]
}

# Define a function to calculate the median of a list of numbers.
proc median {list} {
    set sorted [sort $list]
    set length [llength $sorted]
    if {$length % 2 == 0} {
        # Even number of elements, return the average of the two middle elements.
        set index [expr {($length / 2) - 1}]
        return [expr {([lindex $sorted $index] + [lindex $sorted [incr $index]]) / 2}]
    } else {
        # Odd number of elements, return the middle element.
        set index [expr {$length / 2}]
        return [lindex $sorted $index]
    }
}

# Define a function to calculate the mode of a list of numbers.
proc mode {list} {
    set counts {}
    foreach num $list {
        if {[info exists counts($num)]} {
            incr counts($num)
        } else {
            set counts($num) 1
        }
    }
    set max_count 0
    set modes {}
    foreach num [lsort -integer -decreasing [lkeys $counts]] {
        set count [lindex $counts $num]
        if {$count > $max_count} {
            set modes {}
            lappend modes $num
            set max_count $count
        } elseif {$count == $max_count} {
            lappend modes $num
        }
    }
    return $modes
}

# Define a function to calculate the range of a list of numbers.
proc range {list} {
    if {[llength $list] == 0} {
        return 0
    }
    set min [lindex $list 0]
    set max [lindex $list 0]
    foreach num $list {
        if {$num < $min} {
            set min $num
        }
        if {$num > $max} {
            set max $num
        }
    }
    return [expr {$max - $min}]
}

# Define a function to calculate the standard deviation of a list of numbers.
proc standard_deviation {list} {
    set mean [mean $list]
    set sum_squared_differences 0
    foreach num $list {
        set difference [expr {$num - $mean}]
        set squared_difference [expr {$difference * $difference}]
        set sum_squared_differences [expr {$sum_squared_differences + $squared_difference}]
    }
    set variance [expr {$sum_squared_differences / [llength $list]}]
    return [expr {sqrt($variance)}]
}

# Define a function to calculate the variance of a list of numbers.
proc variance {list} {
    set mean [mean $list]
    set sum_squared_differences 0
    foreach num $list {
        set difference [expr {$num - $mean}]
        set squared_difference [expr {$difference * $difference}]
        set sum_squared_differences [expr {$sum_squared_differences + $squared_difference}]
    }
    return [expr {$sum_squared_differences / [llength $list]}]
}

# Define a function to perform linear regression on a list of data points.
proc linear_regression {data_points} {
    set sum_x 0
    set sum_y 0
    set sum_x_squared 0
    set sum_xy 0
    foreach data_point $data_points {
        set x [lindex $data_point 0]
        set y [lindex $data_point 1]
        set sum_x [expr {$sum_x + $x}]
        set sum_y [expr {$sum_y + $y}]
        set sum_x_squared [expr {$sum_x_squared + ($x * $x)}]
        set sum_xy [expr {$sum_xy + ($x * $y)}]
    }
    set num_data_points [llength $data_points]
    set slope [expr {($num_data_points * $sum_xy - $sum_x * $sum_y) / ($num_data_points * $sum_x_squared - $sum_x * $sum_x)}]
    set intercept [expr {($sum_y - $slope * $sum_x) / $num_data_points}]
    return [list $slope $intercept]
}

# Usage:
# To use the defined functions, you can call them as follows:

# Sort a list of numbers:
set sorted_list [sort {1, 3, 5, 2, 4}]
puts "Sorted list: $sorted_list"

# Search for a value in a list using binary search:
set index [binary_search {1, 3, 5, 2, 4} 3]
if {$index != -1} {
    puts "Value found at index $index"
} else {
    puts "Value not found"
}

# Calculate the mean of a list of numbers:
set list {1, 2, 3, 4, 5}
set mean_value [mean $list]
puts "Mean: $mean_value"

# Calculate the median of a list of numbers:
set median_value [median $list]
puts "Median: $median_value"

# Calculate the mode of a list of numbers:
set list {1, 2, 3, 3, 4, 5, 5, 5}
set modes [mode $list]
puts "Mode(s): $modes"

# Calculate the range of a list of numbers:
set list {1, 3, 5, 2, 4}
set range_value [range $list]
puts "Range: $range_value"

# Calculate the standard deviation of a list of numbers:
set list {1, 2, 3, 4, 5}
set standard_deviation_value [standard_deviation $list]
puts "Standard deviation: $standard_deviation_value"

# Calculate the variance of a list of numbers:
set list {1, 2, 3, 4, 5}
set variance_value [variance $list]
puts "Variance: $variance_value"

# Perform linear regression on a list of data points:
set data_points {{1, 2}, {2, 4}, {3, 6}, {4, 8}, {5, 10}}
set regression_result [linear_regression $data_points]
set slope [lindex $regression_result 0]
set intercept [lindex $regression_result 1]
puts "Linear regression result: slope = $slope, intercept = $intercept"

```

Explanation:

1. The script defines several functions to perform different statistical operations:

    * `sort`: Sorts a list of numbers in ascending order.
    * `binary_search`: Searches for a value in a sorted list using binary search.
    * `mean`: Calculates the mean (average) of a list of numbers.
    * `median`: Calculates the median (middle value) of a list of numbers.
    * `mode`: Calculates the mode (most occurring value) of a list of numbers.
    * `range`: Calculates the range (difference between the maximum and minimum values) of a list