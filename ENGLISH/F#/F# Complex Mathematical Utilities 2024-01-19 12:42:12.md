```f#
// This F# code defines a module called "ComplexCode"
module ComplexCode =

    // Define a recursive function to compute the factorial of a non-negative integer
    let rec factorial n =
        if n = 0 then 1 else n * factorial (n - 1)

    // Define a function to calculate the sum of the first n integers
    let sumOfFirstNIntegers n =
        let rec sumHelper acc i =
            if i <= 0 then acc else sumHelper (acc + i) (i - 1)
        sumHelper 0 n

    // Define a function to find the maximum value in a list of integers
    let maxOfList lst =
        let rec maxHelper mx i =
            match lst with
            | [] -> mx
            | h :: t -> if h > mx then maxHelper h t else maxHelper mx t
        match lst with
        | [] -> failwith "The list is empty"
        | h :: t -> maxHelper h t

    // Define a function to find the prime factors of a positive integer
    let primeFactors n =
        let rec primeFactorsHelper acc i =
            if i > n then acc
            else if n % i = 0 then primeFactorsHelper (i :: acc) (i + 1)
            else primeFactorsHelper acc (i + 1)
        primeFactorsHelper [] 2

    // Define a function to determine if a given integer is prime
    let isPrime n =
        if n < 2 then false
        else primeFactors n = [n]

    // Define a function to find the greatest common divisor (GCD) of two integers
    let gcd a b =
        if b = 0 then a
        else gcd b (a % b)

    // Define a function to find the least common multiple (LCM) of two integers
    let lcm a b =
        let gcdResult = gcd a b
        (a * b) / gcdResult
```

Explanation:

1. `factorial` function: This function calculates the factorial of a given non-negative integer using a recursive approach. It multiplies the number by its previous factorial values until it reaches 0.

2. `sumOfFirstNIntegers` function: This function computes the sum of the first `n` integers. It uses a recursive helper function (`sumHelper`) to accumulate the sum while decrementing `n`.

3. `maxOfList` function: This function finds the maximum value in a list of integers. It uses a recursive helper function (`maxHelper`) to compare elements and keep track of the maximum value.

4. `primeFactors` function: This function calculates the prime factors of a given positive integer. It utilizes a recursive helper function (`primeFactorsHelper`) to divide the number by potential factors and collect the prime factors.

5. `isPrime` function: This function determines if a given integer is prime by checking if its prime factors include only itself.

6. `gcd` function: This function computes the greatest common divisor (GCD) of two integers using the Euclidean algorithm. It repeatedly divides the larger number by the smaller number until the remainder is zero, at which point the last non-zero remainder is the GCD.

7. `lcm` function: This function calculates the least common multiple (LCM) of two integers. It first finds their GCD using the `gcd` function and then divides their product by the GCD to obtain the LCM.