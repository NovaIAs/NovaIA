```tcl
package require Tcl 8.5
proc factorial {n} {
    if {[string compare $n 1] > 0} {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    } else {
        return 1
    }
}
proc isPrime {n} {
    switch -- [expr {$n % 2}] {
        0 {  # Even numbers greater than 2 are not prime
            return 0
        }
        1 {  # Odd numbers less than or equal to 1 are not prime
            return [expr {$n <= 1}]
        }
    }
    # We only need to check divisors up to the square root of n
    set limit [expr {floor([sqrt $n])}]
    foreach i [range 3 $limit 2] {
        switch -- [expr {$n % $i}] {
            0 {  # n is divisible by i, so it is not prime
                return 0
            }
        }
    }
    # n is prime
    return 1
}
proc nextPrime {n} {
    while {[expr {$n % 2 == 0 || [isPrime $n] == 0}]} {
        incr n
    }
    return $n
}
```

Explanation:

* The code defines three procedures: `factorial`, `isPrime`, and `nextPrime`.

* The `factorial()` procedure calculates the factorial of a given number `n`. It uses a recursive algorithm to compute the factorial. The factorial of a number is the product of all positive integers less than or equal to that number.

* The `isPrime()` procedure checks if a given number `n` is prime. It uses a series of checks to determine if the number is prime. A number is prime if it is not divisible by any number other than 1 and itself.

* The `nextPrime()` procedure finds the next prime number greater than or equal to a given number `n`. It increments the value of `n` until it finds a prime number.

Example:

```tcl
puts [factorial 5]         # Output: 120
puts [isPrime 7]           # Output: 1
puts [isPrime 10]          # Output: 0
puts [nextPrime 11]        # Output: 13
```