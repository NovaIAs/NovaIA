```tcl
# This is a complex Tcl script that demonstrates various advanced features of the language.

# Define a procedure to calculate the factorial of a number.
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Define a procedure to generate a list of prime numbers up to a given limit.
proc generatePrimes {limit} {
    set primes {}
    for {set i 2} {$i <= $limit} {incr i} {
        set isPrime 1
        for {set j 2} {$j < $i} {incr j} {
            if {[expr {$i % $j == 0}]} {
                set isPrime 0
                break
            }
        }
        if {$isPrime} {
            lappend primes $i
        }
    }
    return $primes
}

# Define a procedure to calculate the greatest common divisor of two numbers.
proc gcd {a b} {
    while {[expr {$b != 0}]} {
        set temp $b
        set b [expr {$a % $b}]
        set a $temp
    }
    return $a
}

# Define a procedure to calculate the least common multiple of two numbers.
proc lcm {a b} {
    return [expr {$a * $b / [gcd $a $b]}]
}

# Define a procedure to generate a random number between two given limits.
proc random {min max} {
    return [expr {int(rand() * ($max - $min + 1)) + $min}]
}

# Define a procedure to shuffle a list of elements.
proc shuffle {list} {
    set shuffled {}
    while {[llength $list] > 0} {
        set index [random 0 [expr {[llength $list] - 1}]]
        lappend shuffled [lindex $list $index]
        lset list $index ""
    }
    return $shuffled
}

# Define a procedure to sort a list of elements in ascending order.
proc sort {list} {
    set sorted {}
    while {[llength $list] > 0} {
        set min [lindex $list 0]
        set minIndex 0
        for {set i 1} {$i < [llength $list]} {incr i} {
            if {[lstring compare $min [lindex $list $i]] > 0} {
                set min [lindex $list $i]
                set minIndex $i
            }
        }
        lappend sorted $min
        lset list $minIndex ""
    }
    return $sorted
}

# Define a procedure to search for an element in a list.
proc search {list element} {
    for {set i 0} {$i < [llength $list]} {incr i} {
        if {[lstring compare $element [lindex $list $i]] == 0} {
            return $i
        }
    }
    return -1
}

# Define a procedure to reverse a string.
proc reverse {string} {
    set reversed ""
    for {set i [expr {[string length $string] - 1}]} {$i >= 0} {incr i -1} {
        lappend reversed [string index $string $i]
    }
    return $reversed
}

# Define a procedure to convert a string to lowercase.
proc lowercase {string} {
    return [string tolower $string]
}

# Define a procedure to convert a string to uppercase.
proc uppercase {string} {
    return [string toupper $string]
}

# Define a procedure to split a string into a list of words.
proc split {string} {
    set words {}
    set word ""
    for {set i 0} {$i < [string length $string]} {incr i} {
        set char [string index $string $i]
        if {[string is space $char] || [$char == "\n"] || [$char == "\r"]} {
            if {[string length $word] > 0} {
                lappend words $word
            }
            set word ""
        } else {
            append word $char
        }
    }
    if {[string length $word] > 0} {
        lappend words $word
    }
    return $words
}

# Define a procedure to join a list of words into a string.
proc join {list} {
    set string ""
    for {set i 0} {$i < [llength $list]} {incr i} {
        append string [lindex $list $i]
        if {$i < [expr {[llength $list] - 1}]} {
            append string " "
        }
    }
    return $string
}

# Define a procedure to create a dictionary from a list of key-value pairs.
proc createDictionary {list} {
    set dictionary {}
    for {set i 0} {$i < [llength $list]} {incr i 2} {
        set key [lindex $list $i]
        set value [lindex $list [expr {$i + 1}]]
        set dictionary($key) $value
    }
    return $dictionary
}

# Define a procedure to get the value of a key from a dictionary.
proc getDictionaryValue {dictionary key} {
    return $dictionary($key)
}

# Define a procedure to set the value of a key in a dictionary.
proc setDictionaryValue {dictionary key value} {
    set dictionary($key) $value
}

# Define a procedure to remove a key from a dictionary.
proc removeDictionaryKey {dictionary key} {
    unset dictionary($key)
}

# Define a procedure to check if a key exists in a dictionary.
proc hasDictionaryKey {dictionary key} {
    return [info exists dictionary($key)]
}

# Define a procedure to get a list of all the keys in a dictionary.
proc getDictionaryKeys {dictionary} {
    return [array names $dictionary]
}

# Define a procedure to get a list of all the values in a dictionary.
proc getDictionaryValues {dictionary} {
    set values {}
    foreach key [array names $dictionary] {
        lappend values $dictionary($key)
    }
    return $values
}

# Define a procedure to print a dictionary.
proc printDictionary {dictionary} {
    foreach key [array names $dictionary] {
        puts "$key: $dictionary($key)"
    }
}

# Create a list of numbers from 1 to 10.
set numbers [list 1 2 3 4 5 6 7 8 9 10]

# Print the factorial of each number in the list.
puts "Factorials:"
foreach number $numbers {
    puts "$number: [factorial $number]"
}

# Generate a list of prime numbers up to 100.
set primes [generatePrimes 100]