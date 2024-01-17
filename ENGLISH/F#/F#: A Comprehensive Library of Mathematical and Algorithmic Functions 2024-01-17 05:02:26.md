```f#
// Import the necessary libraries
open System
open System.Linq
open System.Collections.Generic

// Define a function to calculate the factorial of a number
let factorial n =
    if n < 0 then failwith "Factorial is not defined for negative numbers"
    else seq {1..n} |> Seq.reduce (*)

// Define a function to calculate the Fibonacci sequence
let fibonacci n =
    if n < 0 then failwith "Fibonacci sequence is not defined for negative numbers"
    else
        let rec fibHelper n acc1 acc2 =
            if n = 0 then acc1
            else if n = 1 then acc2
            else fibHelper (n-1) acc2 (acc1 + acc2)
        fibHelper n 0 1

// Define a function to check if a number is prime
let isPrime n =
    if n < 2 then false
    else
        let rec isPrimeHelper n i =
            if i * i > n then true
            else if n % i = 0 then false
            else isPrimeHelper n (i + 1)
        isPrimeHelper n 2

// Define a function to find the prime factors of a number
let primeFactors n =
    if n < 1 then failwith "Prime factors are not defined for non-positive numbers"
    else
        let rec primeFactorsHelper n factors =
            if n = 1 then factors
            else
                let i = seq {2..n} |> Seq.find (fun i -> n % i = 0)
                primeFactorsHelper (n / i) (factors @ [i])
        primeFactorsHelper n []

// Define a function to calculate the greatest common divisor of two numbers
let gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Define a function to calculate the least common multiple of two numbers
let lcm a b =
    a * b / gcd a b

// Define a function to calculate the sum of the digits of a number
let sumOfDigits n =
    if n < 0 then failwith "Sum of digits is not defined for negative numbers"
    else
        let rec sumOfDigitsHelper n sum =
            if n = 0 then sum
            else sumOfDigitsHelper (n / 10) (sum + (n % 10))
        sumOfDigitsHelper n 0

// Define a function to reverse a string
let reverseString s =
    s |> Seq.toList |> List.reverse |> String

// Define a function to check if a string is a palindrome
let isPalindrome s =
    s = reverseString s

// Define a function to find the longest common subsequence of two strings
let longestCommonSubsequence s1 s2 =
    let n = s1.Length
    let m = s2.Length
    let dp = Array2D.init n m (fun _ _ -> 0)
    for i in 0..(n-1) do
        for j in 0..(m-1) do
            if s1.[i] = s2.[j] then
                dp.[i, j] <- (if i > 0 && j > 0 then dp.[i-1, j-1] else 0) + 1
            else
                dp.[i, j] <- max (if i > 0 then dp.[i-1, j] else 0) (if j > 0 then dp.[i, j-1] else 0)
    dp.[n-1, m-1]

// Define a function to find the shortest common supersequence of two strings
let shortestCommonSupersequence s1 s2 =
    let n = s1.Length
    let m = s2.Length
    let dp = Array2D.init n m (fun _ _ -> 0)
    for i in 0..(n-1) do
        for j in 0..(m-1) do
            if s1.[i] = s2.[j] then
                dp.[i, j] <- (if i > 0 && j > 0 then dp.[i-1, j-1] else 0) + 1
            else
                dp.[i, j] <- max (if i > 0 then dp.[i-1, j] else 0) (if j > 0 then dp.[i, j-1] else 0)
    let sb = new StringBuilder()
    let i = n - 1
    let j = m - 1
    while i >= 0 && j >= 0 do
        if s1.[i] = s2.[j] then
            sb.Append(s1.[i])
            i <- i - 1
            j <- j - 1
        else if dp.[i, j-1] > dp.[i-1, j] then
            sb.Append(s2.[j])
            j <- j - 1
        else
            sb.Append(s1.[i])
            i <- i - 1
    while i >= 0 do
        sb.Append(s1.[i])
        i <- i - 1
    while j >= 0 do
        sb.Append(s2.[j])
        j <- j - 1
    sb.ToString()

// Define a function to find the edit distance between two strings
let editDistance s1 s2 =
    let n = s1.Length
    let m = s2.Length
    let dp = Array2D.init n m (fun _ _ -> 0)
    for i in 0..(n-1) do
        dp.[i, 0] <- i
    for j in 0..(m-1) do
        dp.[0, j] <- j
    for i in 1..(n-1) do
        for j in 1..(m-1) do
            if s1.[i] = s2.[j] then
                dp.[i, j] <- dp.[i-1, j-1]
            else
                dp.[i, j] <- min (dp.[i-1, j]) (dp.[i, j-1]) + 1
    dp.[n-1, m-1]

// Define a function to find the longest increasing subsequence of an array
let longestIncreasingSubsequence arr =
    let n = arr.Length
    let dp = Array.init n 1
    let prev = Array.init n -1
    let maxLength = 1
    let lastIndex = 0
    for i in 1..(n-1) do
        for j in 0..(i-1) do
            if arr.[i] > arr.[j] && dp.[i] < dp.[j] + 1 then
                dp.[i] <- dp.[j] + 1
                prev.[i] <- j
                if dp.[i] > maxLength then
                    maxLength <- dp.[i]
                    lastIndex <- i
    let lis = [|arr.[lastIndex]|]
    while lastIndex > 0 do
        lastIndex <- prev.[lastIndex]
        lis <- arr.[lastIndex] :: lis
    lis |> List.rev

// Define a function to find the longest decreasing subsequence of an array
let longestDecreasingSubsequence arr =
    let n = arr.Length
    let dp = Array.init n 1
    let prev = Array.init n -1
    let maxLength = 1
    let lastIndex = 0
    for i in 1..(n-1) do
        for j in 0..(i-1) do
            if arr.[i] < arr.[j] && dp.[i] < dp.[j] + 1 then
                dp.[i] <- dp.[j] + 1
                prev.[i] <- j
                if dp.[i] > maxLength then
                    maxLength <- dp.[i]
                    lastIndex <- i
    let lds = [|arr.[lastIndex]|]
    while lastIndex > 0 do
        lastIndex <- prev.[lastIndex]
        lds <- arr.[lastIndex] :: lds
    lds |> List.rev

// Define a function to find the maximum subarray sum of an array
let maximumSubarraySum arr =
    let n = arr.Length
    let dp = Array.init n 0
    let maxSum = arr.[0]
    dp.[0] <- arr.[0]
    for i in 1..(n-1) do
        dp.[i] <- max (arr.[i]) (dp.[i-1] + arr.[i])
        maxSum <- max maxSum dp.[i]
    maxSum

// Define a function to find the minimum subarray sum of an array
let minimumSubarraySum arr =
    let n = arr.Length
    let dp = Array.init n 0
    let minSum = arr.[0]
    dp.[0] <- arr.[0]
    for i in 1..(n-1) do
        dp.[i] <- min (arr.[i]) (dp.[i