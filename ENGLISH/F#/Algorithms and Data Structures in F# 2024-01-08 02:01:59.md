```f#
let factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)

let gcd a b =
    match (a, b) with
    | 0, 0 -> 0
    | a, 0 -> a
    | 0, b -> b
    | a, b -> gcd (b mod a) a

let lcm a b =
    a * b / gcd a b

let isPrime n =
    if n < 2 then false
    else
        let rec primeCheck n i =
            if i * i > n then true
            elif n mod i = 0 then false
            else primeCheck n (i + 1)
        in
        primeCheck n 2

let sieveOfEratosthenes n =
    let primes = Array.init (n + 1) true
    primes.[0] <- false
    primes.[1] <- false
    for i = 2 to n do
        if primes.[i] then
            for j = i * i to n by i do
                primes.[j] <- false
    Array.filter id primes

let mergeSort xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let rec merge (xs, ys) =
            match (xs, ys) with
            | [], _ -> ys
            | _, [] -> xs
            | x :: xs', y :: ys' ->
                if x < y then x :: merge (xs', ys)
                else y :: merge (xs, ys')
        in
        let (xs', xs'') = List.splitAt (List.length xs / 2) xs
        merge (mergeSort xs', mergeSort xs'')

let quickSort xs =
    match xs with
    | [] -> []
    | x :: xs' ->
        let (ys, zs) = List.partition (< x) xs'
        quickSort ys @ [x] @ quickSort zs

let binarySearch xs x =
    let rec binarySearch' xs x lo hi =
        if lo > hi then -1
        else
            let mid = (lo + hi) / 2
            match compare x xs.[mid] with
            | 0 -> mid
            | -1 -> binarySearch' xs x lo (mid - 1)
            | 1 -> binarySearch' xs x (mid + 1) hi
    in
    binarySearch' xs x 0 (List.length xs - 1)

let countSort xs =
    let max = List.fold (fun acc x -> max acc x) Int32.MinValue xs
    let count = Array.init (max + 1) 0
    for x in xs do
        count.[x] <- count.[x] + 1
    let sorted = Array.init (List.length xs) 0
    let pos = ref 0
    for i = 0 to max do
        while count.[i] > 0 do
            sorted.[!pos] <- i
            pos := !pos + 1
            count.[i] <- count.[i] - 1
    sorted
```

This code contains a collection of commonly used algorithms and data structures in F#. It includes functions for calculating factorials, finding the greatest common divisor (GCD) and least common multiple (LCM) of two numbers, checking if a number is prime, generating prime numbers using the Sieve of Eratosthenes algorithm, sorting lists using merge sort and quick sort, performing binary search on a sorted list, and sorting a list of integers using counting sort. These algorithms and data structures are essential for various programming tasks and can be used in a wide range of applications.