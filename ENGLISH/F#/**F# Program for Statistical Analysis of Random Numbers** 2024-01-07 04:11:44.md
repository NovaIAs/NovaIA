```f#

// Import the necessary libraries
open System
open System.Linq

// Define a function to generate a list of random numbers
let generateRandomNumbers (n: int) =
    new Random().Next(0, 100)
                |> Seq.init n |> Seq.toList

// Define a function to calculate the average of a list of numbers
let calculateAverage (numbers: int list) =
    numbers.Sum() / numbers.Count

// Define a function to find the maximum value in a list of numbers
let findMaximum (numbers: int list) =
    numbers.Max()

// Define a function to find the minimum value in a list of numbers
let findMinimum (numbers: int list) =
    numbers.Min()

// Define a function to calculate the standard deviation of a list of numbers
let calculateStandardDeviation (numbers: int list) =
    let average = calculateAverage numbers
    let squaredDifferences = numbers
                        |> Seq.map (fun x -> (x - average) ** 2)
                        |> Seq.toList
    let variance = squaredDifferences.Sum() / (numbers.Count - 1)
    Math.Sqrt(variance)

// Define a function to generate a histogram of a list of numbers
let generateHistogram (numbers: int list) (bins: int) =
    let max = findMaximum numbers
    let min = findMinimum numbers
    let range = max - min
    let binSize = range / bins
    let bins = Seq.init bins (fun i -> min + (i * binSize))
                |> Seq.toList
    let histogram = bins
                    |> Seq.map (fun bin ->
                        let values = numbers |> Seq.filter (fun x -> x >= bin && x < (bin + binSize))
                        let count = values.Count
                        (bin, count))
                    |> Seq.toList
    histogram

// Define a function to plot a histogram
let plotHistogram (histogram: (int * int) list) =
    let maxCount = histogram.Max(fun (bin, count) -> count).snd
    let scaleFactor = 100.0 / maxCount
    histogram |> Seq.iter (fun (bin, count) ->
        let stars = new string('*', (int) (count * scaleFactor))
        printfn "%3d: %s" bin stars)

// Generate a list of 100 random numbers
let numbers = generateRandomNumbers 100

// Calculate the average of the numbers
let average = calculateAverage numbers

// Find the maximum value in the numbers
let maximum = findMaximum numbers

// Find the minimum value in the numbers
let minimum = findMinimum numbers

// Calculate the standard deviation of the numbers
let standardDeviation = calculateStandardDeviation numbers

// Generate a histogram of the numbers with 10 bins
let histogram = generateHistogram numbers 10

// Plot the histogram
plotHistogram histogram

```