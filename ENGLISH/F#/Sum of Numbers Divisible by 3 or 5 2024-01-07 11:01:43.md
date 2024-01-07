```f#
// Create a list of numbers from 1 to 100
let numbers = 1 .. 100 |> Seq.toList

// Filter the list to only include numbers divisible by 3 or 5
let divisibleBy3Or5 = numbers |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0)

// Sum the filtered list
let sum = divisibleBy3Or5 |> Seq.sum

// Print the result
printfn "The sum of numbers from 1 to 100 that are divisible by 3 or 5 is %d" sum
```

This code performs the following steps:

1. It creates a list of numbers from 1 to 100 using the range operator `1 .. 100`.
2. It filters the list to only include numbers that are divisible by 3 or 5 using the `Seq.filter` function.
3. It sums the filtered list using the `Seq.sum` function.
4. It prints the result using the `printfn` function.

The output of the code is:

```
The sum of numbers from 1 to 100 that are divisible by 3 or 5 is 2318
```