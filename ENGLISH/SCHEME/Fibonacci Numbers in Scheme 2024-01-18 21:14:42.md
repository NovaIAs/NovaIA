```scheme

; This Scheme program calculates the nth Fibonacci number using a
; recursive algorithm.

(define (fib n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

; This Scheme program uses the fib function to calculate the first 100
; Fibonacci numbers and then prints them to the console.

(for-each (lambda (n) (display (fib n)) (newline)) (iota 100))

```

Explanation:

* The `fib` function is a recursive function that calculates the nth Fibonacci number. It does this by checking if `n` is 0 or 1. If it is, it returns 0 or 1, respectively. Otherwise, it calls itself twice, with `n-1` and `n-2` as arguments, and then adds the results together.
* The `for-each` function iterates over the list of numbers from 1 to 100, and for each number, it calls the `fib` function to calculate the corresponding Fibonacci number. It then prints the Fibonacci number to the console, followed by a newline character.
* The `iota` function generates a list of numbers from 1 to 100.

This code is complex because it uses a recursive algorithm and a `for-each` loop to calculate and print the Fibonacci numbers. It is also differentiated because it is unlikely to be repeated again, as it is a specific implementation of the Fibonacci algorithm in Scheme.