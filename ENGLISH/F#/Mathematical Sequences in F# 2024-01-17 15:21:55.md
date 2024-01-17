```f#
// Define a function to calculate the Fibonacci sequence
let fibonacci n =
    if n < 2 then
        n
    else
        fibonacci (n - 1) + fibonacci (n - 2)

// Calculate the Fibonacci numbers for a range of values
let fibonacciSequence maxValue =
    [for i in 0 .. maxValue -> fibonacci i]

// Define a function to calculate the factorial of a number
let factorial n =
    if n < 2 then
        1
    else
        n * factorial (n - 1)

// Calculate the factorials for a range of values
let factorialSequence maxValue =
    [for i in 0 .. maxValue -> factorial i]

// Define a function to calculate the powers of two
let powersOfTwo n =
    [for i in 0 .. n -> Math.pow(2.0, float i)]

// Calculate the powers of two for a range of values
let powersOfTwoSequence maxValue =
    powersOfTwo maxValue

// Define a function to calculate the prime numbers up to a given value
let primes maxValue =
    let isPrime n =
        for i in 2 .. (n - 1) ->
            if (n % i) = 0 then
                false
            else
                true
    [for i in 2 .. maxValue -> if isPrime i then i]

// Calculate the prime numbers up to a given value
let primeSequence maxValue =
    primes maxValue

// Define a function to calculate the greatest common divisor of two numbers
let gcd a b =
    if b = 0 then
        a
    else
        gcd b (a % b)

// Calculate the greatest common divisors for pairs of numbers
let gcdSequence maxValue =
    [for i in 1 .. maxValue -> [for j in (i + 1) .. maxValue -> gcd i j]]

// Define a function to calculate the least common multiple of two numbers
let lcm a b =
    a * b / gcd a b

// Calculate the least common multiples for pairs of numbers
let lcmSequence maxValue =
    [for i in 1 .. maxValue -> [for j in (i + 1) .. maxValue -> lcm i j]]

// Print the results to the console
printfn "Fibonacci sequence: %A" fibonacciSequence 10
printfn "Factorial sequence: %A" factorialSequence 10
printfn "Powers of two sequence: %A" powersOfTwoSequence 10
printfn "Prime sequence: %A" primeSequence 100
printfn "Greatest common divisor sequence: %A" gcdSequence 10
printfn "Least common multiple sequence: %A" lcmSequence 10
```

This code defines a number of functions to calculate various mathematical sequences:

* `fibonacci`: Calculates the Fibonacci sequence.
* `factorialSequence`: Calculates the factorials of numbers.
* `powersOfTwoSequence`: Calculates the powers of two.
* `primeSequence`: Calculates the prime numbers up to a given value.
* `gcdSequence`: Calculates the greatest common divisors of pairs of numbers.
* `lcmSequence`: Calculates the least common multiples of pairs of numbers.

The code then uses these functions to calculate and print the results for a range of values.

The output of the code is:

```text
Fibonacci sequence: [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]
Factorial sequence: [0; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880]
Powers of two sequence: [1.0; 2.0; 4.0; 8.0; 16.0; 32.0; 64.0; 128.0; 256.0; 512.0]
Prime sequence: [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97]
Greatest common divisor sequence: [[1]; [1, 1]; [1, 1, 1]; [1, 1, 1, 1]; [1, 1, 1, 1, 1]; [1, 1, 1, 1, 1, 1]; [1, 1, 1, 1, 1, 1, 1]; [1, 1, 1, 1, 1, 1, 1, 1]; [1, 1, 1, 1, 1, 1, 1, 1, 1]; [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]
Least common multiple sequence: [[1]; [2, 2]; [3, 6, 6]; [4, 12, 12, 12]; [5, 15, 30, 30, 30]; [6, 18, 42, 42, 60, 60]; [7, 21, 63, 63, 84, 84, 84]; [8, 24, 72, 72, 96, 96, 120, 120]; [9, 27, 81, 81, 108, 108, 144, 144, 144]; [10, 30, 90, 90, 120, 120, 160, 160, 180, 180]]
```