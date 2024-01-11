```f#
// Import the necessary libraries.
open System
open System.Linq
open System.Collections.Generic

// Define a function to generate a random number between two values.
let randomBetween (min, max) =
    let rand = new Random()
    rand.Next(min, max)

// Define a function to generate a random list of integers.
let randomList (length, min, max) =
    let list = new List<int>()
    for i in 1..length do
        list.Add(randomBetween(min, max))
    list

// Define a function to find the maximum value in a list.
let maxOfList list =
    list.Max()

// Define a function to find the minimum value in a list.
let minOfList list =
    list.Min()

// Define a function to calculate the average value of a list.
let avgOfList list =
    let sum = list.Sum()
    sum / list.Count

// Define a function to calculate the median value of a list.
let medianOfList list =
    let sortedList = list.OrderBy(fun x -> x).ToList()
    let length = sortedList.Count
    if length % 2 = 0 then
        (sortedList.[length / 2] + sortedList.[length / 2 - 1]) / 2.0
    else
        sortedList.[length / 2]

// Define a function to calculate the mode value of a list.
let modeOfList list =
    let frequencies = list.GroupBy(fun x -> x).ToDictionary(fun g -> g.Key, fun g -> g.Count())
    let maxValue = frequencies.Values.Max()
    frequencies.Where(fun kvp -> kvp.Value = maxValue).Select(fun kvp -> kvp.Key).ToList()

// Define a function to calculate the standard deviation of a list.
let stdDevOfList list =
    let avg = avgOfList list
    let sumOfSquares = list.Sum(fun x -> (x - avg) * (x - avg))
    Math.Sqrt(sumOfSquares / (list.Count - 1))

// Define a function to calculate the variance of a list.
let varianceOfList list =
    stdDevOfList list * stdDevOfList list

// Define a function to calculate the range of a list.
let rangeOfList list =
    maxOfList list - minOfList list

// Define a function to calculate the interquartile range of a list.
let iqrOfList list =
    let sortedList = list.OrderBy(fun x -> x).ToList()
    let length = sortedList.Count
    let q1 = if length % 2 = 0 then sortedList.[length / 4] else sortedList.[(length / 4) - 1]
    let q3 = if length % 2 = 0 then sortedList.[length * 3 / 4] else sortedList.[(length * 3 / 4) + 1]
    q3 - q1

// Define a function to calculate the skewness of a list.
let skewnessOfList list =
    let avg = avgOfList list
    let stdDev = stdDevOfList list
    let sumOfCubes = list.Sum(fun x -> Math.Pow((x - avg) / stdDev, 3.0))
    sumOfCubes / (list.Count - 1)

// Define a function to calculate the kurtosis of a list.
let kurtosisOfList list =
    let avg = avgOfList list
    let stdDev = stdDevOfList list
    let sumOfFourthPowers = list.Sum(fun x -> Math.Pow((x - avg) / stdDev, 4.0))
    sumOfFourthPowers / (list.Count - 1)

// Define a function to calculate the Jarque-Bera statistic for a list.
let jarqueBeraOfList list =
    let skewness = skewnessOfList list
    let kurtosis = kurtosisOfList list
    Math.Pow(skewness, 2.0) + Math.Pow(kurtosis - 3.0, 2.0)

// Define a function to calculate the Shapiro-Wilk statistic for a list.
let shapiroWilkOfList list =
    let sortedList = list.OrderBy(fun x -> x).ToList()
    let n = sortedList.