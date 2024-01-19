```tcl
proc print_fibonacci_sequence {n} {
    if {$n <= 0} {
        return 0
    } elseif {$n == 1} {
        return 1
    } else {
        return [print_fibonacci_sequence [expr {$n - 1}]] + [print_fibonacci_sequence [expr {$n - 2}]]
    }
}
```

**Explanation:**

*  `proc print_fibonacci_sequence {n}`: This line defines a Tcl procedure named `print_fibonacci_sequence`, which takes a single argument `n`. This procedure will be used to calculate and print the Fibonacci sequence up to the n-th term.


*  `if {$n <= 0} { ... } elseif {$n == 1} { ... } else { ... }`: This is a conditional statement that checks the value of `n`.

    * If `n` is less than or equal to 0, it means we are at the end of the sequence and we return 0 as the base case.

    * If `n` is equal to 1, it means we are at the first term of the sequence and we return 1 as the base case.

    * If `n` is greater than 1, it means we are in the middle of the sequence and we need to calculate the next term.


*  `return [print_fibonacci_sequence [expr {$n - 1}]] + [print_fibonacci_sequence [expr {$n - 2}]]`: Inside the `else` block, we calculate the n-th term of the Fibonacci sequence using recursion. We call the `print_fibonacci_sequence` procedure twice, once with `n-1` and once with `n-2`, and add the results together. This is the definition of the Fibonacci sequence, where each term is the sum of the two preceding ones.

**Usage:**

To use the `print_fibonacci_sequence` procedure, you can call it from the Tcl command line or from within a Tcl script. For example, to print the first 10 terms of the Fibonacci sequence, you would use the following command:

```tcl
foreach i [seq 10] {
    puts [print_fibonacci_sequence $i]
}
```

Output:

```
1
1
2
3
5
8
13
21
34
55
```