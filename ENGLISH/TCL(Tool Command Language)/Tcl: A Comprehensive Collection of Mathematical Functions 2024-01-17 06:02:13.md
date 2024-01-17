```tcl
# Define a procedure to calculate the factorial of a number
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Define a procedure to convert a number from base 10 to any other base
proc baseConvert {number base} {
    if {$number == 0} {
        return ""
    } else {
        set remainder [expr {$number % $base}]
        set quotient [expr {$number / $base}]
        return [baseConvert $quotient $base] [format %a $remainder]
    }
}

# Define a procedure to check if a number is prime
proc isPrime {n} {
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

# Define a procedure to find the greatest common divisor of two numbers
proc gcd {a b} {
    if {$b == 0} {
        return $a
    } else {
        return [gcd $b [expr {$a % $b}]]
    }
}

# Define a procedure to find the least common multiple of two numbers
proc lcm {a b} {
    return [expr {$a * $b / [gcd $a $b]}]
}

# Define a procedure to generate a random number between two numbers
proc randRange {min max} {
    return [expr {floor($min + [rand() * ($max - $min + 1)])}]
}

# Define a procedure to shuffle a list
proc shuffle {list} {
    set shuffled {}
    foreach item $list {
        set index [randRange 0 [llength $shuffled]]
        lset shuffled $index $item
    }
    return $shuffled
}

# Define a procedure to sort a list
proc sort {list} {
    if {[llength $list] <= 1} {
        return $list
    }
    set pivot [lindex $list [expr {[llength $list] / 2}]]
    set less {}
    set greater {}
    foreach item $list {
        if {$item < $pivot} {
            lappend less $item
        } elseif {$item > $pivot} {
            lappend greater $item
        }
    }
    return [sort $less] [sort $greater]
}

# Define a procedure to find the median of a list
proc median {list} {
    set sorted [sort $list]
    set length [llength $sorted]
    if {[expr {$length % 2}] == 1} {
        return [lindex $sorted [expr {($length - 1) / 2}]]
    } else {
        set lower [lindex $sorted [expr {($length - 1) / 2}]]
        set upper [lindex $sorted [expr {($length + 1) / 2}]]
        return [expr {($lower + $upper) / 2}]
    }
}

# Define a procedure to find the mode of a list
proc mode {list} {
    set counts {}
    foreach item $list {
        if {[info exist counts($item)] == 0} {
            set counts($item) 0
        }
        incr counts($item)
    }
    set maxCount 0
    set mode {}
    foreach item [lsort -integer -decreasing [lkeys $counts]] {
        set count $counts($item)
        if {$count > $maxCount} {
            set maxCount $count
            set mode $item
        }
    }
    return $mode
}

# Define a procedure to find the range of a list
proc range {list} {
    set min [lindex $list 0]
    set max [lindex $list 0]
    foreach item $list {
        if {$item < $min} {
            set min $item
        }
        if {$item > $max} {
            set max $item
        }
    }
    return [list $min $max]
}

# Define a procedure to find the standard deviation of a list
proc stddev {list} {
    set mean [mean $list]
    set sumSquares 0
    foreach item $list {
        set diff [expr {$item - $mean}]
        set sumSquares [expr {$sumSquares + ($diff * $diff