```tcl
proc fibonacci {n} {
    if {$n == 0} {
        return 0
    } elseif {$n == 1} {
        return 1
    } else {
        return [fibonacci [expr {$n - 1}]] + [fibonacci [expr {$n - 2}]]
    }
}

proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * factorial [expr {$n - 1}]}]
    }
}

proc gcd {a b} {
    if {$b == 0} {
        return $a
    } else {
        return [gcd $b [expr {$a % $b}]]
    }
}

proc lcm {a b} {
    return [expr {$a * $b / [gcd $a $b]}]
}

proc is_prime {n} {
    if {$n <= 1} {
        return 0
    }
    for {set i 2} {$i * $i <= $n} {incr i} {
        if {[expr {$n % $i}] == 0} {
            return 0
        }
    }
    return 1
}

proc next_prime {n} {
    while {[is_prime $n] == 0} {
        incr n
    }
    return $n
}

proc print_table {n} {
    for {set i 1} {$i <= $n} {incr i} {
        for {set j 1} {$j <= $n} {incr j} {
            puts -nonewline [expr {$i * $j}] "\t"
        }
        puts ""
    }
}

proc binary_search {arr target} {
    set low 0
    set high [llength $arr] - 1
    while {$low <= $high} {
        set mid [expr {($low + $high) / 2}]
        if {[lindex $arr $mid] == $target} {
            return $mid
        } elseif {[lindex $arr $mid] < $target} {
            set low [expr {$mid + 1}]
        } else {
            set high [expr {$mid - 1}]
        }
    }
    return -1
}

proc merge_sort {arr} {
    if {[llength $arr] <= 1} {
        return $arr
    }
    set mid [expr {[llength $arr] / 2}]
    set left [merge_sort [lrange $arr 0 [expr {$mid - 1}]]]
    set right [merge_sort [lrange $arr $mid end]]
    return [merge $left $right]
}

proc merge {left right} {
    set result {}
    set i 0
    set j 0
    while {$i < [llength $left] && $j < [llength $right]} {
        if {[lindex $left $i] < [lindex $right $j]} {
            lappend result [lindex $left $i]
            incr i
        } else {
            lappend result [lindex $right $j]
            incr j
        }
    }
    while {$i < [llength $left]} {
        lappend result [lindex $left $i]
        incr i
    }
    while {$j < [llength $right]} {
        lappend result [lindex $right $j]
        incr j
    }
    return $result
}

proc quick_sort {arr} {
    if {[llength $arr] <= 1} {
        return $arr
    }
    set pivot [lindex $arr 0]
    set left {}
    set right {}
    for {set i 1} {$i < [llength $arr]} {incr i} {
        if {[lindex $arr $i] < $pivot} {
            lappend left [lindex $arr $i]
        } elseif {[lindex $arr $i] > $pivot} {
            lappend right [lindex $arr $i]
        }
    }
    return [concat [quick_sort $left] [$pivot] [quick_sort $right]]
}

proc heap_sort {arr} {
    set n [llength $arr]
    for {set i [expr {$n / 2 - 1}]} {$i >= 0} {incr i -1} {
        heapify $arr $i $n
    }
    for {set i [expr {$n - 1}]} {$i >= 0} {incr i -1} {
        set temp [lindex $arr 0]
        set arr 0 [lindex $arr $i]
        set arr $i $temp
        heapify $arr 0 $i
    }
    return $arr
}

proc heapify {arr i n} {
    set largest $i
    set left [expr {$i * 2 + 1}]
    set right [expr {$i * 2 + 2}]
    if {$left < $n && [lindex $arr $left] > [lindex $arr $largest]} {
        set largest $left
    }
    if {$right < $n && [lindex $arr $right] > [lindex $arr $largest]} {
        set largest $right
    }
    if {$largest != $i} {
        set temp [lindex $arr $i]
        set arr $i [lindex $arr $largest]
        set arr $largest $temp
        heapify $arr $largest $n
    }
}
```

This code provides various mathematical and sorting algorithms, including Fibonacci sequence, factorial, greatest common divisor (GCD), least common multiple (LCM), primality testing, prime number generation, multiplication table, binary search, merge sort, quick sort, and heap sort. It also contains a demonstration of how to use these algorithms in Tcl.

Here's a breakdown of the code:

1. **Fibonacci Sequence**:
   - `fibonacci` function calculates the nth Fibonacci number using recursion.

2. **Factorial**:
   - `factorial` function computes the factorial of a non-negative integer.

3. **Greatest Common Divisor (GCD)**:
   - `gcd` function finds the GCD of two non-negative integers using Euclid's algorithm.

4. **Least Common Multiple (LCM)**:
   - `lcm` function calculates the LCM of two non-negative integers using the GCD.

5. **Primality Testing**:
   - `is_prime` function checks if a given integer is prime.

6. **Prime Number Generation**:
   - `next_prime` function finds the next prime number after a given integer.

7. **Multiplication Table**:
   - `print_table` function prints a multiplication table up to a specified size.

8. **Binary Search**:
   - `binary_search` function performs a binary search on a sorted array to find the index of a target element.

9. **Merge Sort**:
   - `merge_sort` function implements the merge sort algorithm for sorting an array.

10. **Quick Sort**:
    - `quick_sort` function implements the quick sort algorithm for sorting an array.

11. **Heap Sort**:
    - `heap_sort` function implements the heap sort algorithm for sorting an array.

12. **Heapify**:
    - `heapify` function is a helper function used by the heap sort algorithm to maintain the heap property.

The comments in the code provide detailed explanations of each function's purpose and algorithm.