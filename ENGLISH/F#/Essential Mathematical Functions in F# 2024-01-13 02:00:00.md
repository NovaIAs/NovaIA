```f#
// Define a function to calculate the factorial of a non-negative integer
let factorial n =
    if n = 0 then
        1
    else
        n * factorial (n - 1)

// Define a function to generate a Fibonacci sequence up to a specified length
let fibonacci length =
    if length <= 0 then
        []
    elif length = 1 then
        [0]
    else
        [0; 1] @ List.init (length - 2) (fun i -> fibonacci (i + 1) |> List.last + fibonacci (i + 2) |> List.last)

// Define a function to check if a given number is prime
let isPrime n =
    if n <= 1 then
        false
    elif n = 2 then
        true
    else
        let rec isPrimeHelper d =
            if d * d > n then
                true
            elif n % d = 0 then
                false
            else
                isPrimeHelper (d + 2)
        isPrimeHelper 3

// Define a function to generate a list of prime numbers up to a specified limit
let primes limit =
    let rec primesHelper n primes =
        if n > limit then
            primes
        elif isPrime n then
            primesHelper (n + 2) (n :: primes)
        else
            primesHelper (n + 2) primes
    primesHelper 3 []

// Define a function to calculate the sum of the squares of the first n positive integers
let sumOfSquares n =
    List.init n (fun i -> i * i) |> List.sum

// Define a function to calculate the product of the first n positive integers
let productOfN n =
    List.init n (fun i -> i + 1) |> List.fold (*) 1

// Define a function to calculate the greatest common divisor of two non-negative integers
let gcd a b =
    if b = 0 then
        a
    else
        gcd b (a % b)

// Define a function to calculate the least common multiple of two non-negative integers
let lcm a b =
    a * b / gcd a b

// Define a function to check if two strings are anagrams of each other
let isAnagram a b =
    let sortedA = String.sort (fun x y -> compare x y) a
    let sortedB = String.sort (fun x y -> compare x y) b
    sortedA = sortedB

// Define a function to find the longest common substring of two strings
let longestCommonSubstring a b =
    let rec longestCommonSubstringHelper i j =
        if i >= String.length a || j >= String.length b then
            ""
        elif String.get a i = String.get b j then
            String.get a i + longestCommonSubstringHelper (i + 1) (j + 1)
        else
            let lcs1 = longestCommonSubstringHelper i (j + 1)
            let lcs2 = longestCommonSubstringHelper (i + 1) j
            if String.length lcs1 > String.length lcs2 then
                lcs1
            else
                lcs2
    longestCommonSubstringHelper 0 0

// Define a function to find the longest palindromic substring of a given string
let longestPalindromicSubstring a =
    let rec longestPalindromicSubstringHelper i j =
        if i > j then
            ""
        elif i = j then
            Char.ToString a.[i]
        elif String.get a i = String.get a j then
            if j - i <= 2 then
                String.substring a i (j - i + 1)
            else
                let lps1 = longestPalindromicSubstringHelper (i + 1) (j - 1)
                if String.length lps1 > 0 then
                    lps1
                else
                    String.substring a i (j - i + 1)
        else
            let lps1 = longestPalindromicSubstringHelper i (j - 1)
            let lps2 = longestPalindromicSubstringHelper (i + 1) j
            if String.length lps1 > String.length lps2 then
                lps1
            else
                lps2
    longestPalindromicSubstringHelper 0 (String.length a - 1)
```