```f#
// Define the main module
module Main =

    // Define a function to calculate the factorial of a number
    let factorial n =
        if n = 0 then 1
        else n * factorial (n - 1)

    // Define a function to calculate the nth Fibonacci number
    let fibonacci n =
        if n <= 1 then n
        else fibonacci (n - 1) + fibonacci (n - 2)

    // Define a function to check if a number is prime
    let isPrime n =
        if n <= 1 then false
        else
            let rec isPrimeHelper n divisor =
                if divisor * divisor > n then true
                else if n mod divisor = 0 then false
                else isPrimeHelper n (divisor + 1)

            isPrimeHelper n 2

    // Define a function to find the greatest common divisor of two numbers
    let gcd a b =
        if b = 0 then a
        else gcd b (a % b)

    // Define a function to find the least common multiple of two numbers
    let lcm a b =
        a * b / gcd a b

    // Define a function to generate a list of prime numbers up to a given limit
    let primes limit =
        let rec sieve primesToTest primesFound limit =
            if primesToTest.IsEmpty then primesFound
            else
                let nextPrime = primesToTest.Head
                let primesFiltered = primesToTest |> List.filter (fun n -> n <> nextPrime && n mod nextPrime <> 0)
                sieve primesFiltered (primesFound @ [nextPrime]) limit

        sieve [2 .. limit] [] limit

    // Define a function to generate a list of all the factors of a number
    let factors n =
        let rec factorsHelper n factors divisor =
            if divisor * divisor > n then factors
            else if n mod divisor = 0 then factorsHelper (n / divisor) (factors @ [divisor]) (divisor + 1)
            else factorsHelper n factors (divisor + 1)

        factorsHelper n [] 2

    // Define a function to check if a number is perfect
    let isPerfect n =
        let factorsOfN = factors n
        factorsOfN.Sum() - n = n

    // Define a function to find the sum of the proper divisors of a number
    let sumOfProperDivisors n =
        let factorsOfN = factors n
        factorsOfN.Sum() - n

    // Define a function to find the sum of the squares of the first n natural numbers
    let sumOfSquares n =
        let rec sumOfSquaresHelper n sum =
            if n = 0 then sum
            else sumOfSquaresHelper (n - 1) (sum + n * n)

        sumOfSquaresHelper n 0

    // Define a function to find the sum of the cubes of the first n natural numbers
    let sumOfCubes n =
        let rec sumOfCubesHelper n sum =
            if n = 0 then sum
            else sumOfCubesHelper (n - 1) (sum + n * n * n)

        sumOfCubesHelper n 0

    // Define a function to find the sum of the factorials of the first n natural numbers
    let sumOfFactorials n =
        let rec sumOfFactorialsHelper n sum =
            if n = 0 then sum
            else sumOfFactorialsHelper (n - 1) (sum + factorial n)

        sumOfFactorialsHelper n 0

    // Define a function to find the sum of the Fibonacci numbers up to the nth term
    let sumOfFibonacci n =
        let rec sumOfFibonacciHelper n sum =
            if n <= 1 then sum
            else sumOfFibonacciHelper (n - 1) (sum + fibonacci n)

        sumOfFibonacciHelper n 0

    // Define a function to find the sum of the prime numbers up to a given limit
    let sumOfPrimes limit =
        let primesToLimit = primes limit
        primesToLimit.Sum()

    // Define a function to find the product of the prime numbers up to a given limit
    let productOfPrimes limit =
        let primesToLimit = primes limit
        primesToLimit.Product()

    // Define a function to find the sum of the perfect numbers up to a given limit
    let sumOfPerfectNumbers limit =
        let rec sumOfPerfectNumbersHelper n sum =
            if n > limit then sum
            else if isPerfect n then sumOfPerfectNumbersHelper (n + 1) (sum + n)
            else sumOfPerfectNumbersHelper (n + 1) sum

        sumOfPerfectNumbersHelper 2 0

    // Define a function to find the product of the perfect numbers up to a given limit
    let productOfPerfectNumbers limit =
        let rec productOfPerfectNumbersHelper n product =
            if n > limit then product
            else if isPerfect n then productOfPerfectNumbersHelper (n + 1) (product * n)
            else productOfPerfectNumbersHelper (n + 1) product

        productOfPerfectNumbersHelper 2 1

    // Define a function to find the sum of the abundant numbers up to a given limit
    let sumOfAbundantNumbers limit =
        let rec sumOfAbundantNumbersHelper n sum =
            if n > limit then sum
            else if sumOfProperDivisors n > n then sumOfAbundantNumbersHelper (n + 1) (sum + n)
            else sumOfAbundantNumbersHelper (n + 1) sum

        sumOfAbundantNumbersHelper 12 0

    // Define a function to find the product of the abundant numbers up to a given limit
    let productOfAbundantNumbers limit =
        let rec productOfAbundantNumbersHelper n product =
            if n > limit then product
            else if sumOfProperDivisors n > n then productOfAbundantNumbersHelper (n + 1) (product * n)
            else productOfAbundantNumbersHelper (n + 1) product

        productOfAbundantNumbersHelper 12 1

    // Define a function to find the sum of the deficient numbers up to a given limit
    let sumOfDeficientNumbers limit =
        let rec sumOfDeficientNumbersHelper n sum =
            if n > limit then sum
            else if sumOfProperDivisors n < n then sumOfDeficientNumbersHelper (n + 1) (sum + n)
            else sumOfDeficientNumbersHelper (n + 1) sum

        sumOfDeficientNumbersHelper 1 0

    // Define a function to find the product of the deficient numbers up to a given limit
    let productOfDeficientNumbers limit =
        let rec productOfDeficientNumbersHelper n product =
            if n > limit then product
            else if sumOfProperDivisors n < n then productOfDeficientNumbersHelper (n + 1) (product * n)
            else productOfDeficientNumbersHelper (n + 1) product

        productOfDeficientNumbersHelper 1 1

```