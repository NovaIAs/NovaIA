```tcl
# Define a procedure to calculate the factorial of a number
proc factorial {n} {
    if {$n <= 1} {
        return 1
    } else {
        return [expr {$n * [factorial [expr {$n - 1}]]}]
    }
}

# Define a procedure to generate a list of prime numbers up to a given limit
proc primes {limit} {
    set primes {}
    for {set i 2} {$i <= $limit} {incr i} {
        set is_prime 1
        for {set j 2} {$j < $i} {incr j} {
            if {[expr {$i % $j}] == 0} {
                set is_prime 0
                break
            }
        }
        if {$is_prime == 1} {
            lappend primes $i
        }
    }
    return $primes
}

# Define a procedure to calculate the sum of the digits of a number
proc sum_digits {n} {
    set sum 0
    while {$n > 0} {
        set digit [expr {$n % 10}]
        set sum [expr {$sum + $digit}]
        set n [expr {$n / 10}]
    }
    return $sum
}

# Define a procedure to find the longest common substring of two strings
proc longest_common_substring {str1 str2} {
    set len1 [string length $str1]
    set len2 [string length $str2]
    set lcs ""
    for {set i 0} {$i < $len1} {incr i} {
        for {set j 0} {$j < $len2} {incr j} {
            set substring [string range $str1 $i end]
            if {[string index $str2 $j] == $substring} {
                set lcs $substring
                break
            }
        }
    }
    return $lcs
}

# Use the procedures to perform some calculations
set factorial_10 [factorial 10]
puts "Factorial of 10: $factorial_10"

set prime_numbers [primes 100]
puts "Prime numbers up to 100: $prime_numbers"

set sum_digits_1234 [sum_digits 1234]
puts "Sum of the digits of 1234: $sum_digits_1234"

set lcs_helloworld_goodbye [longest_common_substring "helloworld" "goodbye"]
puts "Longest common substring of 'helloworld' and 'goodbye': $lcs_helloworld_goodbye"
```

This code performs a variety of mathematical and string-related calculations using procedures defined in Tcl. Here's a breakdown of the code:

1. **factorial** Procedure:
   - This procedure calculates the factorial of a given number `n`.
   - It uses recursion to calculate the factorial, with a base case for `n <= 1` and a recursive case for `n > 1`.

2. **primes** Procedure:
   - This procedure generates a list of prime numbers up to a given limit.
   - It starts with an empty list of primes and iterates through numbers from 2 to the limit.
   - For each number `i`, it checks if it's prime by testing divisibility with all numbers from 2 to `i-1`. If `i` is prime, it's added to the list.

3. **sum_digits** Procedure:
   - This procedure calculates the sum of the digits of a given number `n`.
   - It repeatedly extracts the last digit of `n`, adds it to the sum, and divides `n` by 10 to remove the last digit.
   - This process continues until `n` becomes 0, at which point the sum is returned.

4. **longest_common_substring** Procedure:
   - This procedure finds the longest common substring between two strings `str1` and `str2`.
   - It iterates through the characters of `str1` and `str2`, checking if there's a match between the current character and the substring starting at the current position in the other string.
   - If a match is found, it updates the longest common substring.
   - The procedure returns the longest common substring found during the iteration.

5. **Usage of the Procedures**:
   - The code demonstrates the usage of these procedures by performing various calculations and printing the results.
   - It calculates the factorial of 10, generates a list of prime numbers up to 100, finds the sum of the digits of 1234, and finds the longest common substring between "helloworld" and "goodbye".
   - The results of these calculations are printed to the console using the `puts` command.