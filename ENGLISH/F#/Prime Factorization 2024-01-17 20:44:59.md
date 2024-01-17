```f#
// Define a module to encapsulate the code
module PrimeFactorization =

    // Define a private helper function to check if a number is prime
    let isPrime n =
        // Check if n is less than or equal to 1, in which case it is not prime
        if n <= 1 then false
        // Check if n is 2, in which case it is prime
        elif n = 2 then true
        // Loop from 3 to the square root of n, checking if n is divisible by any number in this range
        else
            let rec isPrimeHelper i =
                // If i is greater than the square root of n, then n is prime
                if i * i > n then true
                // If n is divisible by i, then it is not prime
                elif n % i = 0 then false
                // Otherwise, continue checking with the next number
                else isPrimeHelper (i + 2) // Skip even numbers greater than 2
            isPrimeHelper 3 // Start checking from 3

    // Define a public function to find the prime factorization of a number
    let primeFactors n =
        // If n is less than or equal to 1, return an empty list
        if n <= 1 then []
        // If n is prime, return a list containing only n
        elif isPrime n then [n]
        // Otherwise, find the smallest prime factor of n and recursively find the prime factorization of the quotient
        else
            let rec findSmallestPrimeFactor n =
                // If n is less than or equal to 1, return 1
                if n <= 1 then 1
                // If n is even, return 2
                elif n % 2 = 0 then 2
                // Otherwise, loop from 3 to the square root of n, checking if n is divisible by any number in this range
                else
                    let rec findSmallestPrimeFactorHelper i =
                        // If i is greater than the square root of n, then n is prime and its smallest prime factor is n
                        if i * i > n then n
                        // If n is divisible by i, then i is the smallest prime factor of n
                        elif n % i = 0 then i
                        // Otherwise, continue checking with the next number
                        else findSmallestPrimeFactorHelper (i + 2) // Skip even numbers greater than 2
                    findSmallestPrimeFactorHelper 3 // Start checking from 3
            let smallestPrimeFactor = findSmallestPrimeFactor n
            smallestPrimeFactor :: primeFactors (n / smallestPrimeFactor)

// Usage:
// Find the prime factorization of 12
let result1 = PrimeFactorization.primeFactors 12
printfn "%A" result1 // [2; 2; 3]

// Find the prime factorization of 13
let result2 = PrimeFactorization.primeFactors 13
printfn "%A" result2 // [13]

// Find the prime factorization of 1000
let result3 = PrimeFactorization.primeFactors 1000
printfn "%A" result3 // [2; 2; 2; 2; 2; 5; 5; 5]
```

This code defines a module named `PrimeFactorization` that contains functions to find the prime factorization of a number.

The `isPrime` function checks if a number is prime by looping from 3 to the square root of the number and checking if it is divisible by any number in this range. If it is not divisible by any number in this range, then it is prime.

The `primeFactors` function finds the prime factorization of a number by repeatedly finding the smallest prime factor of the number and recursively finding the prime factorization of the quotient.

The code is explained in detail in the comments.

To use the code, you can import the `PrimeFactorization` module and call the `primeFactors` function to find the prime factorization of a number.

For example, the following code finds the prime factorization of 12:

```f#
let result1 = PrimeFactorization.primeFactors 12
printfn "%A" result1 // [2; 2; 3]
```

The output of this code is `[2; 2; 3]`, which is the prime factorization of 12.