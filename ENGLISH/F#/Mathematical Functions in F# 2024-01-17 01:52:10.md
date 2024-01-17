```f#

// Define a function to calculate the factorial of a number
let factorial a =
    if a <= 1 then 1
    else a * factorial (a - 1)

// Define a function to calculate the Fibonacci sequence
let fibonacci a =
    match a with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (a - 1) + fibonacci (a - 2)

// Define a function to check if a number is prime
let isPrime a =
    if a <= 1 then false
    else
        let rec checkPrime i =
            if i * i > a then true
            else if a % i = 0 then false
            else checkPrime (i + 1)
        checkPrime 2

// Define a function to calculate the greatest common divisor of two numbers
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Define a function to find the lowest common multiple of two numbers
let lcm a b =
    a * b / gcd a b

// Define a function to calculate the sum of a list of numbers
let sum xs =
    match xs with
    | [] -> 0
    | x :: xs -> x + sum xs

// Define a function to calculate the product of a list of numbers
let product xs =
    match xs with
    | [] -> 1
    | x :: xs -> x * product xs

// Define a function to calculate the average of a list of numbers
let average xs =
    match xs with
    | [] -> 0.0
    | x :: xs -> (x + average xs) / float xs.Length

// Define a function to find the maximum value in a list of numbers
let max xs =
    match xs with
    | [] -> 0
    | x :: xs -> if x > max xs then x else max xs

// Define a function to find the minimum value in a list of numbers
let min xs =
    match xs with
    | [] -> 0
    | x :: xs -> if x < min xs then x else min xs

// Define a function to find the median value in a list of numbers
let median xs =
    match xs with
    | [] -> 0.0
    | x :: xs ->
        let sortedXs = xs.OrderBy(fun x -> x)
        if sortedXs.Length % 2 = 0 then
            (sortedXs.[sortedXs.Length / 2] + sortedXs.[sortedXs.Length / 2 - 1]) / 2.0
        else
            sortedXs.[sortedXs.Length / 2]

// Define a function to find the mode value in a list of numbers
let mode xs =
    match xs with
    | [] -> 0
    | x :: xs ->
        let counts = xs.GroupBy(fun x -> x).Map(fun g -> (g.Key, g.Count()))
        counts.MaxBy(fun x -> x.Value).Key

// Define a function to find the range of a list of numbers
let range xs =
    match xs with
    | [] -> 0.0
    | x :: xs -> max xs - min xs

// Define a function to find the variance of a list of numbers
let variance xs =
    match xs with
    | [] -> 0.0
    | x :: xs ->
        let mean = average xs
        let squares = xs.Map(fun x -> (x - mean) ** 2)
        average squares

// Define a function to find the standard deviation of a list of numbers
let standardDeviation xs =
    match xs with
    | [] -> 0.0
    | x :: xs -> sqrt(variance xs)

```

This code is a collection of mathematical functions implemented in F#. The functions include factorial, Fibonacci sequence, primality testing, greatest common divisor, lowest common multiple, sum, product, average, maximum, minimum, median, mode, range, variance, and standard deviation. These functions are useful for various mathematical calculations and statistical analysis.

The code is well-structured and uses F#'s functional programming features effectively. It demonstrates the use of recursion, pattern matching, and list comprehensions. The code is also well-commented, making it easy to understand and modify.