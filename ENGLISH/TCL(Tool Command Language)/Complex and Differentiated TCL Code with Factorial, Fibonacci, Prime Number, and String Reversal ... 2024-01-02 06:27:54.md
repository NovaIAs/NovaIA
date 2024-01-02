Certainly! Here's a complex and differentiated code written in TCL:

```tcl
# Function to calculate the factorial of a number
proc factorial {num} {
    if {$num <= 1} {
        return 1
    } else {
        return [expr {$num * [factorial [expr $num - 1]]}]
    }
}

# Function to find the nth Fibonacci number
proc fibonacci {n} {
    if {$n <= 1} {
        return $n
    } else {
        return [expr {[fibonacci [expr $n - 1]] + [fibonacci [expr $n - 2]]}]
    }
}

# Function to check if a number is prime
proc is_prime {num} {
    if {$num <= 1} {
        return 0
    } elseif {$num <= 3} {
        return 1
    } elseif {![expr {$num%2}]} {
        return 0
    }
    
    set i 5
    while {[expr {$i*$i <= $num}]} {
        if {![expr {$num%$i}]} {
            return 0
        }
        if {![expr {$num%[expr $i+2]}]} {
            return 0
        }
        incr i 6
    }
    
    return 1
}

# Function to reverse a string
proc reverse_string {str} {
    set reversed ""
    for {set i [expr {[string length $str] - 1}]} {$i >= 0} {incr i -1} {
        append reversed [string index $str $i]
    }
    return $reversed
}

# Main program
set number 10
puts "Factorial of $number is [factorial $number]"
puts "10th Fibonacci number is [fibonacci 10]"

if {[is_prime $number]} {
    puts "$number is a prime number"
} else {
    puts "$number is not a prime number"
}

set string "Hello, World!"
puts "Reversed string: [reverse_string $string]"
```

In this code, I have implemented four different functions:

1. `factorial`: This function calculates the factorial of a given number using recursion.
2. `fibonacci`: This function finds the nth Fibonacci number using recursion.
3. `is_prime`: This function checks if a number is prime or not using a primality test algorithm.
4. `reverse_string`: This function reverses a given string using a for loop.

The main program demonstrates the usage of these functions. It calculates the factorial of 10, finds the 10th Fibonacci number, checks if 10 is a prime number, and reverses the string "Hello, World!". The results are then printed to the console.

Feel free to modify or enhance this code according to your requirements.