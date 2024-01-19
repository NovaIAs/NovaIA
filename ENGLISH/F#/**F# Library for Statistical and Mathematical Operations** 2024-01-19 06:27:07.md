```f#
// Define a recursive function to calculate the Fibonacci sequence
let rec Fibonacci (n: int) =
    if n <= 1 then
        n
    else
        Fibonacci (n - 1) + Fibonacci (n - 2)

// Define a function to generate a list of Fibonacci numbers up to a specified limit
let GenerateFibonacciSequence (n: int) =
    let fibSequence = List.init n (fun i -> Fibonacci i)
    fibSequence

// Define a function to calculate the sum of a list of numbers
let Sum (numbers: int list) =
    List.fold (fun (acc, x) -> acc + x) (0, 0) numbers

// Define a function to calculate the average of a list of numbers
let Average (numbers: int list) =
    let sum = Sum numbers
    sum / List.length numbers

// Define a function to find the maximum value in a list of numbers
let Maximum (numbers: int list) =
    List.fold (fun (acc, x) -> max acc x) Int32.MinValue numbers

// Define a function to find the minimum value in a list of numbers
let Minimum (numbers: int list) =
    List.fold (fun (acc, x) -> min acc x) Int32.MaxValue numbers

// Define a function to count the number of occurrences of a value in a list
let Count (value: int) (numbers: int list) =
    List.count (fun x -> x = value) numbers

// Define a function to remove duplicate values from a list
let RemoveDuplicates (numbers: int list) =
    List.distinct numbers

// Define a function to sort a list of numbers in ascending order
let SortAscending (numbers: int list) =
    List.sortWith (fun x y -> x - y) numbers

// Define a function to sort a list of numbers in descending order
let SortDescending (numbers: int list) =
    List.sortWith (fun x y -> y - x) numbers

// Define a function to reverse a list
let Reverse (numbers: int list) =
    List.rev numbers

// Define a function to find the median of a list of numbers
let Median (numbers: int list) =
    let sortedNumbers = SortAscending numbers
    let length = List.length sortedNumbers
    if length % 2 = 1 then
        sortedNumbers.[length / 2]
    else
        (sortedNumbers.[length / 2 - 1] + sortedNumbers.[length / 2]) / 2

// Define a function to find the mode of a list of numbers
let Mode (numbers: int list) =
    let sortedNumbers = SortAscending numbers
    let counts = List.map (fun x -> Count x sortedNumbers) sortedNumbers
    let maxCount = Maximum counts
    let modes = List.filter (fun (x, count) -> count = maxCount) (List.zip sortedNumbers counts)
    modes

// Define a function to find the range of a list of numbers
let Range (numbers: int list) =
    Maximum numbers - Minimum numbers

// Define a function to find the variance of a list of numbers
let Variance (numbers: int list) =
    let mean = Average numbers
    let squaredDifferences = List.map (fun x -> (x - mean) * (x - mean)) numbers
    Sum squaredDifferences / (List.length numbers - 1)

// Define a function to find the standard deviation of a list of numbers
let StandardDeviation (numbers: int list) =
    sqrt (Variance numbers)

// Usage:
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

printfn "Fibonacci sequence: %A" (GenerateFibonacciSequence 10)
printfn "Sum: %A" (Sum numbers)
printfn "Average: %A" (Average numbers)
printfn "Maximum: %A" (Maximum numbers)
printfn "Minimum: %A" (Minimum numbers)
printfn "Count of 5: %A" (Count 5 numbers)
printfn "Removed duplicates: %A" (RemoveDuplicates numbers)
printfn "Sorted ascending: %A" (SortAscending numbers)
printfn "Sorted descending: %A" (SortDescending numbers)
printfn "Reversed: %A" (Reverse numbers)
printfn "Median: %A" (Median numbers)
printfn "Mode: %A" (Mode numbers)
printfn "Range: %A" (Range numbers)
printfn "Variance: %A" (Variance numbers)
printfn "Standard deviation: %A" (StandardDeviation numbers)
```

This F# code provides a comprehensive set of functions for performing common statistical and mathematical operations on a list of numbers. It includes functions for calculating the Fibonacci sequence, finding the sum, average, maximum, minimum, and range of a list, counting the occurrences of a value, removing duplicates, sorting in ascending and descending order, reversing the list, finding the median, mode, and standard deviation. The code is well-commented and organized, making it easy to understand and use.