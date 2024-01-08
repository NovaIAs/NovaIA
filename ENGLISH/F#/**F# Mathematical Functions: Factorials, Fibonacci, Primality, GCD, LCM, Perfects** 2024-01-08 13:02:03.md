```f#
// A function to calculate the factorial of a number
let factorial n =
    match n with
    | 0 -> 1
    | n -> n * factorial (n-1)

// A function to calculate the nth Fibonacci number
let fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib (n-2)

// A function to check if a number is prime
let isPrime n =
    if n < 2 then false
    else
        let rec primeCheck n d =
            if d * d > n then true
            else if n mod d = 0 then false
            else primeCheck n (d+1)
        primeCheck n 2

// A function to generate a list of prime numbers up to a certain number
let primes n =
    let rec primeList n primes =
        if n <= 2 then primes
        else if isPrime n then primeList (n-1) (n::primes)
        else primeList (n-1) primes
    primeList n []

// A function to calculate the greatest common divisor of two numbers
let gcd a b =
    if b = 0 then a
    else gcd b (a mod b)

// A function to calculate the least common multiple of two numbers
let lcm a b =
    a * b / gcd a b

// A function to check if a number is perfect
let isPerfect n =
    let sum =
        let rec sumDivisors n d =
            if d * d > n then 0
            else if n mod d = 0 then d + sumDivisors n (d+1)
            else sumDivisors n (d+1)
        sumDivisors n 2
    sum = n

// A function to generate a list of perfect numbers up to a certain number
let perfects n =
    let rec perfectList n perfects =
        if n <= 1 then perfects
        else if isPerfect n then perfectList (n-1) (n::perfects)
        else perfectList (n-1) perfects
    perfectList n []
```

This code is a collection of various mathematical functions, including functions to calculate factorials, Fibonacci numbers, check for prime numbers, generate lists of prime numbers, calculate the greatest common divisor and least common multiple of two numbers, check for perfect numbers, and generate lists of perfect numbers. The code is well-commented and easy to understand, even for those who are not familiar with F#.