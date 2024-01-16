```tcl
# Define a procedure to calculate the factorial of a number
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Define a procedure to calculate the Fibonacci sequence up to a given number
proc fibonacci {n} {
    if {$n <= 1} {
        return $n
    } else {
        return [expr {[fibonacci [expr {$n - 1}]] + [fibonacci [expr {$n - 2}]]}]
    }
}

# Define a procedure to check if a number is prime
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

# Define a procedure to generate a list of prime numbers up to a given number
proc prime_list {n} {
    set primes {}
    for {set i 2} {$i <= $n} {incr i} {
        if {[is_prime $i]} {
            lappend primes $i
        }
    }
    return $primes
}

# Define a procedure to find the greatest common divisor of two numbers
proc gcd {a b} {
    if {$b == 0} {
        return $a
    }
    return [gcd $b [expr {$a % $b}]]
}

# Define a procedure to find the least common multiple of two numbers
proc lcm {a b} {
    return [expr {$a * $b / [gcd $a $b]}]
}

# Define a procedure to calculate the area of a triangle
proc triangle_area {a b c} {
    set s [expr {($a + $b + $c) / 2}]
    return [expr {sqrt([expr {$s * ($s - $a) * ($s - $b) * ($s - $c)}])}]
}

# Define a procedure to calculate the volume of a sphere
proc sphere_volume {r} {
    return [expr {4/3 * pi * [expr {$r * $r * $r}]}]
}

# Define a procedure to calculate the surface area of a sphere
proc sphere_surface_area {r} {
    return [expr {4 * pi * [expr {$r * $r}]}]
}

# Define a procedure to calculate the determinant of a matrix
proc matrix_determinant {matrix} {
    if {[llength $matrix] != [llength [lindex $matrix 0]]} {
        error "Matrix must be square"
    }
    if {[llength $matrix] == 1} {
        return [lindex $matrix 0 0]
    }
    set det 0
    for {set i 0} {$i < [llength $matrix]} {incr i} {
        set minor [matrix_minor $matrix $i 0]
        set sign [expr {(-1) ** $i}]
        set det [expr {$det + [expr {$sign * [matrix_determinant $minor]}]]}
    }
    return $det
}

# Define a procedure to calculate the minor of a matrix
proc matrix_minor {matrix row column} {
    set minor {}
    for {set i 0} {$i < [llength $matrix]} {incr i} {
        if {$i != $row} {
            set minor [lconcat [lappend minor [lrange [lindex $matrix $i] 0 $column]] [lrange [lindex $matrix $i] [expr {$column + 1}] end]]]
        }
    }
    return $minor
}

# Define a procedure to calculate the inverse of a matrix
proc matrix_inverse {matrix} {
    set det [matrix_determinant $matrix]
    if {$det == 0} {
        error "Matrix is not invertible"
    }
    set cofactor_matrix {}
    for {set i 0} {$i < [llength $matrix]} {incr i} {
        for {set j 0} {$j < [llength $matrix]} {incr j} {
            set minor [matrix_minor $matrix $i $j]
            set sign [expr {(-1) ** [expr {$i + $j}]}]
            lappend cofactor_matrix [expr {$sign * [matrix_determinant $minor]}]]
        }
    }
    set adjoint_matrix [matrix_transpose $cofactor_matrix]
    return [matrix_multiply $adjoint_matrix [expr {1 / $det}]]
}

# Define a procedure to calculate the transpose of a matrix
proc matrix_transpose {matrix} {
    set transposed_matrix {}
    for {set i 0} {$i < [llength $matrix]} {incr i} {
        for {set j 0} {$j < [llength $matrix]} {incr j} {
            lappend transposed_matrix [lindex [lindex $matrix $j] $i]
        }
    }
    return $transposed_matrix
}

# Define a procedure to calculate the product of two matrices
proc matrix_multiply {matrix1 matrix2} {
    if {[llength [lindex $matrix1 0]] != [llength $matrix2]} {
        error "Matrices cannot be multiplied"
    }
    set product_matrix {}
    for {set i 0} {$i < [llength $matrix1]} {incr i} {
        set row {}
        for {set j 0} {$j < [llength $matrix2]} {incr j} {
            set dot_product 0
            for {set k 0} {$k < [llength [lindex $matrix1 0]]} {incr k} {
                set dot_product [expr {$dot_product + [expr {[lindex [lindex $matrix1 $i] $k] * [lindex [lindex $matrix2 $k] $j]}]]}]
            }
            lappend row $dot_product
        }
        lappend product_matrix $row
    }
    return $product_matrix
}

# Define a procedure to solve a system of linear equations using Gaussian elimination
proc gauss_elimination {matrix} {
    set augmented_matrix $matrix
    for {set i 0} {$i < [llength $augmented_matrix]} {incr i} {
        # Find the pivot element in the current column
        set pivot_row $i
        for {set j [expr {$i + 1}]} {$j < [llength $augmented_matrix]} {incr j} {
            if {abs([lindex [lindex $augmented_matrix $j]