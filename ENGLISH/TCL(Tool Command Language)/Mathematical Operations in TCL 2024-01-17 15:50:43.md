## Complex TCL (Tool Command Language) Code
```tcl
proc factorial {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
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
    return [expr {$a * $b} / [gcd $a $b]]
}

proc is_prime {n} {
    if {$n < 2} {
        return 0
    }

    for {set i 2} {$i * $i <= $n} {incr i} {
        if {[expr {$n % $i}] == 0} {
            return 0
        }
    }

    return 1
}

proc nth_prime {n} {
    set count 0
    set prime 2
    while {$count < $n} {
        if [is_prime $prime] {
            incr count
            if {$count == $n} {
                break
            }
        }
        incr prime
    }
    return $prime
}

puts [factorial 10]
puts [gcd 12 18]
puts [lcm 12 18]
puts [is_prime 13]
puts [nth_prime 100]
```
## Explanation
- The factorial procedure calculates the factorial of a number.
- The gcd procedure calculates the greatest common divisor of two numbers.
- The lcm procedure calculates the least common multiple of two numbers.
- The is_prime procedure checks if a number is prime.
- The nth_prime procedure returns the nth prime number.

### Usage
```tcl
% factorial 10
3628800
% gcd 12 18
6
% lcm 12 18
36
% is_prime 13
1
% nth_prime 100
541
```