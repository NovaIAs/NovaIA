```f#
let rec factorial n =
    match n with
    | 0 -> 1
    | _ -> n * factorial (n - 1)

// Calculate the Fibonacci sequence up to a given number of terms
let rec fib n =
    match n with
    | 0 | 1 -> n
    | _ -> fib (n - 1) + fib (n - 2)

// Sort a list of integers using the quicksort algorithm
let rec quicksort xs =
    match xs with
    | [] -> []
    | x::xs ->
        let smaller, larger = xs |> List.partition (fun y -> y < x)
        quicksort smaller @ x :: quicksort larger

// Find the maximum value in a list of integers
let max xs =
    match xs with
    | [] -> failwith "Cannot find the maximum value of an empty list"
    | [x] -> x
    | x::xs -> max [x; max xs]

// Find the minimum value in a list of integers
let min xs =
    match xs with
    | [] -> failwith "Cannot find the minimum value of an empty list"
    | [x] -> x
    | x::xs -> min [x; min xs]

// Calculate the sum of a list of integers
let sum xs =
    match xs with
    | [] -> 0
    | x::xs -> x + sum xs

// Calculate the product of a list of integers
let product xs =
    match xs with
    | [] -> 1
    | x::xs -> x * product xs

// Reverse a list
let rec reverse xs =
    match xs with
    | [] -> []
    | x::xs -> reverse xs @ [x]

// Check if a list is palindrome
let isPalindrome xs =
    xs = reverse xs

// Find the index of the first occurrence of an element in a list
let indexOf x xs =
    match xs with
    | [] -> -1
    | y::ys when x = y -> 0
    | y::ys -> 1 + indexOf x ys

// Find the index of the last occurrence of an element in a list
let lastIndexOf x xs =
    match xs with
    | [] -> -1
    | y::ys when x = y -> length xs - 1
    | y::ys -> lastIndexOf x ys

// Remove the first occurrence of an element from a list
let removeFirst x xs =
    match xs with
    | [] -> []
    | y::ys when x = y -> ys
    | y::ys -> y :: removeFirst x ys

// Remove the last occurrence of an element from a list
let removeLast x xs =
    match xs with
    | [] -> []
    | y::ys when x = y && ys = [] -> []
    | y::ys when x = y -> removeLast x ys
    | y::ys -> y :: removeLast x ys

// Find the intersection of two lists
let intersection xs ys =
    xs |> List.filter (fun x -> ys |> List.contains x)

// Find the union of two lists
let union xs ys =
    xs |> List.filter (fun x -> not (ys |> List.contains x)) @ ys

// Find the difference of two lists
let difference xs ys =
    xs |> List.filter (fun x -> not (ys |> List.contains x))

// Find the symmetric difference of two lists
let symmetricDifference xs ys =
    difference xs ys @ difference ys xs

// Check if a list is a subset of another list
let isSubset xs ys =
    xs |> List.forall (fun x -> ys |> List.contains x)

// Check if two lists are equal
let areEqual xs ys =
    xs |> List.zip ys |> List.forall (fun (x, y) -> x = y)

// Find the greatest common divisor of two integers
let gcd x y =
    match y with
    | 0 -> abs x
    | _ -> gcd y (x % y)

// Find the least common multiple of two integers
let lcm x y =
    abs x * abs y / gcd x y

// Check if a number is prime
let isPrime n =
    let rec isPrime' n i =
        if i * i > n then true
        elif n % i = 0 then false
        else isPrime' n (i + 1)
    isPrime' n 2

// Find the prime factors of a number
let primeFactors n =
    let rec primeFactors' n i =
        if n = 1 then []
        elif i * i > n then [n]
        elif n % i = 0 then i :: primeFactors' (n / i) i
        else primeFactors' n (i + 1)
    primeFactors' n 2
```