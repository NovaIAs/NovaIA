```f#
// Create a function to calculate the Fibonacci sequence
let rec fib n =
    match n with
    | 0 | 1 -> 1
    | _ -> fib(n-1) + fib(n-2)

// Create a function to check if a number is prime
let isPrime n =
    for i in 2..Math.sqrt(float n) do
        if n % i = 0 then return false
    return true

// Create a function to calculate the factorial of a number
let factorial n =
    if n = 0 then 1
    else n * factorial(n-1)

// Create a function to calculate the sum of a list of numbers
let sum lst =
    List.fold (+) 0 lst

// Create a function to reverse a list
let reverse lst =
    List.fold (fun acc x -> x :: acc) [] lst

// Create a function to sort a list of numbers
let sort lst =
    List.sortWith (fun a b -> a - b) lst

// Print the results
printfn "Fibonacci of 10: %A" (fib 10)
printfn "Is 13 prime: %B" (isPrime 13)
printfn "Factorial of 5: %A" (factorial 5)
printfn "Sum of [1, 2, 3, 4, 5]: %A" (sum [1; 2; 3; 4; 5])
printfn "Reversed list [1, 2, 3, 4, 5]: %A" (reverse [1; 2; 3; 4; 5])
printfn "Sorted list [5, 1, 3, 2, 4]: %A" (sort [5; 1; 3; 2; 4])
```

This code defines a number of functions that perform various mathematical operations, such as calculating the Fibonacci sequence, checking if a number is prime, calculating the factorial of a number, calculating the sum of a list of numbers, reversing a list, and sorting a list of numbers. The code then uses these functions to perform various operations and prints the results.