```f#
// Define a function to calculate the factorial of a number
let factorial n =
    if n = 0 then
        1
    else
        n * factorial (n-1)

// Define a function to calculate the nth Fibonacci number
let fibonacci n =
    if n <= 1 then
        n
    else
        fibonacci (n-1) + fibonacci (n-2)

// Define a function to check if a number is prime
let isPrime n =
    if n < 2 then
        false
    else if n = 2 then
        true
    else
        let rec isPrime' n i =
            if i = 1 then
                true
            else if n % i = 0 then
                false
            else
                isPrime' n (i-1)
        isPrime' n (n-1)

// Define a function to find the greatest common divisor of two numbers
let gcd a b =
    if b = 0 then
        a
    else
        gcd b (a % b)

// Define a function to find the least common multiple of two numbers
let lcm a b =
    a * b / gcd a b

// Define a function to generate a random number between 0 and 1
let random =
    System.Random().NextDouble

// Define a function to generate a random integer between 0 and n-1
let randomInt n =
    System.Random().Next(n)

// Define a function to shuffle a list
let shuffle lst =
    let rec shuffle' lst shuffled =
        if lst = [] then
            shuffled
        else
            let i = randomInt (List.length lst)
            let x = List.item i lst
            shuffle' (List.removeAt i lst) (x :: shuffled)
    shuffle' lst []

// Define a function to sort a list of numbers in ascending order
let sort lst =
    List.sort (fun a b -> a - b) lst

// Define a function to find the maximum value in a list of numbers
let max lst =
    List.fold (fun a b -> if a > b then a else b) lst

// Define a function to find the minimum value in a list of numbers
let min lst =
    List.fold (fun a b -> if a < b then a else b) lst

// Define a function to find the sum of a list of numbers
let sum lst =
    List.fold (fun a b -> a + b) lst

// Define a function to find the average of a list of numbers
let average lst =
    sum lst / List.length lst

// Define a function to find the median of a list of numbers
let median lst =
    let sorted = sort lst
    if List.length sorted % 2 = 1 then
        sorted.[List.length sorted / 2]
    else
        (sorted.[List.length sorted / 2 - 1] + sorted.[List.length sorted / 2]) / 2.0

// Define a function to find the mode of a list of numbers
let mode lst =
    let freqs =
        List.fold
            (fun acc x ->
                let count = acc.[x] + 1
                acc.Add(x, count))
            (Map.empty)
            lst
    let maxFreq =
        List.fold
            (fun maxFreq x ->
                if freqs.[x] > maxFreq then
                    freqs.[x]
                else
                    maxFreq)
            0
            freqs.Keys
    let modes =
        List.filter
            (fun x -> freqs.[x] = maxFreq)
            freqs.Keys
    modes

// Define a function to find the standard deviation of a list of numbers
let standardDeviation lst =
    let mean = average lst
    let sumOfSquares =
        List.fold
            (fun acc x -> acc + (x - mean) * (x - mean))
            0.0
            lst
    Math.Sqrt(sumOfSquares / List.length lst)
```

This code contains a collection of complex and frequently used functions in F#. Each function addresses a specific mathematical or computational problem, ranging from basic operations like factorial and Fibonacci calculation to advanced statistical measures like standard deviation.

Here's a brief explanation of each function:

1. **Factorial**: Calculates the factorial of a number.

2. **Fibonacci**: Computes the nth Fibonacci number.

3. **Is Prime**: Determines if a number is prime.

4. **GCD**: Finds the greatest common divisor of two numbers.

5. **LCM**: Calculates the least common multiple of two numbers.

6. **Random**: Generates a random number between 0 and 1.

7. **Random Int**: Generates a random integer between 0 and n-1.

8. **Shuffle**: Randomizes the order of elements in a list.

9. **Sort**: Sorts a list of numbers in ascending order.

10. **Max**: Finds the maximum value in a list of numbers.

11. **Min**: Finds the minimum value in a list of numbers.

12. **Sum**: Calculates the sum of all elements in a list of numbers.

13. **Average**: Computes the average of all elements in a list of numbers.

14. **Median**: Finds the median value of a list of numbers.

15. **Mode**: Determines the most frequently occurring value in a list of numbers.

16. **Standard Deviation**: Calculates the standard deviation of a list of numbers.

Overall, this code provides a comprehensive set of mathematical and statistical functions that can be useful in various programming scenarios.