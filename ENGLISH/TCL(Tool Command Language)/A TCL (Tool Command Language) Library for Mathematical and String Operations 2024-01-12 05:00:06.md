```tcl
proc factorial {n} {
    if {$n == 0} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}

proc fibonacci {n} {
    if {$n < 2} {
        return $n
    }
    return [expr {[fibonacci [expr {$n - 1}]] + [fibonacci [expr {$n - 2}]]}]
}

proc gcd {a b} {
    if {$b == 0} {
        return $a
    }
    return [gcd $b [expr {$a % $b}]]
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
    while {[is_prime [incr n]] == 0} {}
    return $n
}

proc print_primes {n} {
    for {set i 2} {$i <= $n} {incr i} {
        if {[is_prime $i]} {
            puts $i
        }
    }
}

proc reverse_string {str} {
    if {[string length $str] == 0} {
        return ""
    }
    return [reverse_string [string range $str 1 end]] + [string range $str 0 0]
}

proc find_substring {str sub} {
    for {set i 0} {$i < [string length $str]} {incr i} {
        if {[string compare [string range $str $i end] $sub] == 0} {
            return $i
        }
    }
    return -1
}

proc count_substring {str sub} {
    set count 0
    for {set i 0} {$i < [string length $str]} {incr i} {
        if {[string compare [string range $str $i end] $sub] == 0} {
            incr count
        }
    }
    return $count
}

proc remove_substring {str sub} {
    set new_str ""
    for {set i 0} {$i < [string length $str]} {incr i} {
        if {[string compare [string range $str $i end] $sub] != 0} {
            set new_str [concat $new_str [string range $str $i $i]]
        }
    }
    return $new_str
}

proc replace_substring {str sub new_sub} {
    set new_str ""
    for {set i 0} {$i < [string length $str]} {incr i} {
        if {[string compare [string range $str $i end] $sub] == 0} {
            set new_str [concat $new_str $new_sub]
        } else {
            set new_str [concat $new_str [string range $str $i $i]]
        }
    }
    return $new_str
}

proc split_string {str delim} {
    set list {}
    set start 0
    for {set i 0} {$i < [string length $str]} {incr i} {
        if {[string compare [string range $str $i $i] $delim] == 0} {
            lappend list [string range $str $start [expr {$i - 1}]]
            set start [expr {$i + 1}]
        }
    }
    lappend list [string range $str $start end]
    return $list
}

proc join_string {list delim} {
    set str ""
    foreach item $list {
        if {$str != ""} {
            set str [concat $str $delim]
        }
        set str [concat $str $item]
    }
    return $str
}

proc sort_list {list} {
    set sorted_list {}
    while {[llength $list] > 0} {
        set min [lsearch $list {*}]
        lappend sorted_list [lindex $list $min]
        lset list $min ""
    }
    return $sorted_list
}

proc reverse_list {list} {
    set reversed_list {}
    for {set i [llength $list]} {$i >= 0} {decr i} {
        lappend reversed_list [lindex $list $i]
    }
    return $reversed_list
}

proc find_element {list element} {
    for {set i 0} {$i < [llength $list]} {incr i} {
        if {[string compare [lindex $list $i] $element] == 0} {
            return $i
        }
    }
    return -1
}

proc remove_element {list element} {
    set new_list {}
    for {set i 0} {$i < [llength $list]} {incr i} {
        if {[string compare [lindex $list $i] $element] != 0} {
            lappend new_list [lindex $list $i]
        }
    }
    return $new_list
}

proc insert_element {list element index} {
    set new_list {}
    for {set i 0} {$i < [llength $list]} {incr i} {
        if {$i == $index} {
            lappend new_list $element
        }
        lappend new_list [lindex $list $i]
    }
    if {$index == [llength $list]} {
        lappend new_list $element
    }
    return $new_list
}

proc merge_lists {list1 list2} {
    set merged_list {}
    foreach item $list1 {
        lappend merged_list $item
    }
    foreach item $list2 {
        lappend merged_list $item
    }
    return $merged_list
}

proc intersect_lists {list1 list2} {
    set intersect_list {}
    foreach item $list1 {
        if {[llength [lsearch -exact $list2 $item]] > 0} {
            lappend intersect_list $item
        }
    }
    return $intersect_list
}

proc union_lists {list1 list2} {
    set union_list {}
    foreach item $list1 {
        lappend union_list $item
    }
    foreach item $list2 {
        if {[llength [lsearch -exact $union_list $item]] == 0} {
            lappend union_list $item
        }
    }
    return $union_list
}

proc difference_lists {list1 list2} {
    set difference_list {}
    foreach item $list1 {
        if {[llength [lsearch -exact $list2 $item]] == 0} {
            lappend difference_list $item
        }
    }
    return $difference_list
}

proc symmetric_difference_lists {list1 list2} {
    set symmetric_difference_list {}
    foreach item $list1 {
        if {[llength [lsearch -exact $list2 $item]] == 0} {
            lappend symmetric_difference_list $item
        }
    }
    foreach item $list2 {
        if {[llength [lsearch -exact $list1 $item]] == 0} {
            lappend symmetric_difference_list $item
        }
    }
    return $symmetric_difference_list
}

proc cartesian_product {list1 list2} {
    set product_list {}
    foreach item1 $list1 {
        foreach item2 $list2 {
            lappend product_list [list $item1 $item2]
        }
    }
    return $product_list
}

proc permutations {list} {
    if {[llength $list] == 0} {
        return {{}}
    }
    set permutations {}
    foreach item $list {
        set remaining_list [remove_element $list $item]
        set sub_permutations [permutations $remaining_list]
        foreach sub_permutation $sub_permutations {
            lappend permutations [concat [list $item] $sub_permutation]
        }
    }
    return $permutations
}

proc combinations {list r} {
    if {$r == 0} {
        return {{}}
    }
    