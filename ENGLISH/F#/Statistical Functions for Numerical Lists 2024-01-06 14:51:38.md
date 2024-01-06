```f#
// This is a function that takes a list of numbers and returns the sum of the numbers.
let sum (numbers: int list) =
    numbers |> List.fold (fun acc num -> acc + num) 0

// This is a function that takes a list of numbers and returns the average of the numbers.
let average (numbers: int list) =
    let sum = numbers |> sum
    sum / List.length numbers

// This is a function that takes a list of numbers and returns the median of the numbers.
let median (numbers: int list) =
    let sortedNumbers = numbers |> List.sort
    let length = List.length sortedNumbers
    if length % 2 = 0 then
        (sortedNumbers.[length / 2 - 1] + sortedNumbers.[length / 2]) / 2
    else
        sortedNumbers.[length / 2]

// This is a function that takes a list of numbers and returns the mode of the numbers.
let mode (numbers: int list) =
    let frequencies = numbers |> List.groupBy id
    let maxFrequency = frequencies |> List.maxBy (fun kvp -> kvp.Value.Length)
    maxFrequency.Key

// This is a function that takes a list of numbers and returns the range of the numbers.
let range (numbers: int list) =
    let sortedNumbers = numbers |> List.sort
    sortedNumbers.[List.length sortedNumbers - 1] - sortedNumbers.[0]

// This is a function that takes a list of numbers and returns the variance of the numbers.
let variance (numbers: int list) =
    let mean = average numbers
    let squaredDifferences = numbers |> List.map (fun num -> (num - mean) ** 2)
    squaredDifferences |> sum / List.length numbers

// This is a function that takes a list of numbers and returns the standard deviation of the numbers.
let standardDeviation (numbers: int list) =
    Math.Sqrt variance numbers
```

Explanation:

The first function, `sum`, takes a list of numbers and returns the sum of the numbers. It uses the `List.fold` function to iterate over the list and add each number to the previous sum.

The second function, `average`, takes a list of numbers and returns the average of the numbers. It uses the `sum` function to calculate the sum of the numbers and then divides the sum by the length of the list.

The third function, `median`, takes a list of numbers and returns the median of the numbers. It first sorts the list of numbers and then calculates the middle value of the sorted list. If the length of the list is even, the median is the average of the two middle values.

The fourth function, `mode`, takes a list of numbers and returns the mode of the numbers. It first groups the numbers by their value and then finds the group with the maximum number of values. The key of the group with the maximum number of values is the mode of the numbers.

The fifth function, `range`, takes a list of numbers and returns the range of the numbers. It first sorts the list of numbers and then subtracts the first value from the last value in the sorted list.

The sixth function, `variance`, takes a list of numbers and returns the variance of the numbers. It first calculates the mean of the numbers and then calculates the squared differences between each number and the mean. It then sums the squared differences and divides the sum by the length of the list.

The seventh function, `standardDeviation`, takes a list of numbers and returns the standard deviation of the numbers. It uses the `variance` function to calculate the variance of the numbers and then takes the square root of the variance.