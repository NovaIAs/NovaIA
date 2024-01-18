```tcl
proc generate_prime_numbers {n} {
    set primes {}
    set i 2
    while {[llength $primes] < $n} {
        if {[is_prime $i]} {
            lappend primes $i
        }
        incr i
    }
    return $primes
}

proc is_prime {n} {
    if {[llength $n] == 1} {
        return 1
    }
    set root [floor [sqrt $n]]
    for {set i 2} {$i <= $root} {incr i} {
        if {[expr {$n % $i} == 0]} {
            return 0
        }
    }
    return 1
}

proc factor_number {n} {
    set factors {}
    set divisor 2
    while {[expr {$n % $divisor} == 0]} {
        lappend factors $divisor
        set n [expr {$n / $divisor}]
    }
    incr divisor
    while {[expr {$n % $divisor} != 0] && {$divisor <= [sqrt $n]}} {
        incr divisor
    }
    if {$divisor <= [sqrt $n]} {
        lappend factors $divisor
        set n [expr {$n / $divisor}]
    }
    if {$n > 1} {
        lappend factors $n
    }
    return $factors
}

proc find_common_factors {numbers} {
    set common_factors {}
    for {set i 0} {$i < [llength $numbers]} {incr i} {
        set number [lindex $numbers $i]
        set factors [factor_number $number]
        if {[llength $common_factors] == 0} {
            set common_factors $factors
        } else {
            set common_factors [intersect $common_factors $factors]
        }
    }
    return $common_factors
}

set numbers {12 18 24}
set common_factors [find_common_factors $numbers]
puts "The common factors of the numbers $numbers are: $common_factors"
```

Explanation:

1. The `generate_prime_numbers` procedure generates a list of prime numbers up to a specified limit `n`. It starts with an empty list of primes and iterates through numbers starting from 2. For each number `i`, it checks if it is prime using the `is_prime` procedure. If `i` is prime, it is added to the list of primes. The procedure stops when the length of the prime list reaches `n`.

2. The `is_prime` procedure checks if a given number `n` is prime. It returns 1 if `n` is prime and 0 otherwise. It first checks if `n` is a single-digit number, in which case it is prime by definition. Otherwise, it iterates through numbers from 2 to the square root of `n` and checks if `n` is divisible by any of these numbers. If `n` is divisible by any of these numbers, it is not prime and the procedure returns 0. Otherwise, it returns 1.

3. The `factor_number` procedure finds the prime factors of a given number `n`. It starts with an empty list of factors and iterates through numbers starting from 2. For each number `i`, it checks if `n` is divisible by `i`. If `n` is divisible by `i`, `i` is added to the list of factors and `n` is divided by `i`. The procedure stops when `n` is 1 or when the divisor `i` is greater than the square root of `n`. If `n` is greater than 1 at the end of the loop, it means that `n` is a prime number and it is added to the list of factors.

4. The `find_common_factors` procedure finds the common factors of a list of numbers. It starts with an empty list of common factors and iterates through the numbers in the list. For each number, it finds the prime factors using the `factor_number` procedure. If the list of common factors is empty, it is initialized with the prime factors of the first number. Otherwise, the common factors are intersected with the prime factors of the current number. This ensures that only the factors that are common to all numbers in the list are included in the list of common factors.

5. The last part of the code demonstrates how to use the above procedures to find the common factors of a list of numbers. It defines a list of numbers `numbers` and calls the `find_common_factors` procedure to find the common factors of these numbers. The list of common factors is then printed to the console.