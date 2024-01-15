```f#
// Calculate the mean and standard deviation of a list of numbers.

// Define a function to calculate the mean of a list of numbers.
let mean xs =
    let sum = List.fold (+) 0 xs
    sum / float (List.length xs)

// Define a function to calculate the standard deviation of a list of numbers.
let standardDeviation xs =
    let m = mean xs
    let diffs = List.map (fun x -> x - m) xs
    let squares = List.map (fun x -> x * x) diffs
    let sumSquares = List.fold (+) 0 squares
    sqrt (sumSquares / float (List.length xs - 1))

// Define a function to calculate the skewness of a list of numbers.
let skewness xs =
    let m = mean xs
    let sd = standardDeviation xs
    let cubes = List.map (fun x -> pow (x - m, 3.0)) xs
    let sumCubes = List.fold (+) 0 cubes
    sumCubes / (float (List.length xs) * pow sd, 3.0)

// Define a function to calculate the kurtosis of a list of numbers.
let kurtosis xs =
    let m = mean xs
    let sd = standardDeviation xs
    let fourthPowers = List.map (fun x -> pow (x - m, 4.0)) xs
    let sumFourthPowers = List.fold (+) 0 fourthPowers
    sumFourthPowers / (float (List.length xs) * pow sd, 4.0) - 3.0

// Define a function to calculate the summary statistics of a list of numbers.
let summaryStatistics xs =
    printfn "Mean: %f" (mean xs)
    printfn "Standard deviation: %f" (standardDeviation xs)
    printfn "Skewness: %f" (skewness xs)
    printfn "Kurtosis: %f" (kurtosis xs)

// Define a list of numbers.
let xs = [1.0; 2.0; 3.0; 4.0; 5.0]

// Calculate the summary statistics of the list of numbers.
summaryStatistics xs
```

This code defines a number of functions to calculate various summary statistics for a list of numbers. These statistics include the mean, standard deviation, skewness, and kurtosis. The code also defines a function to calculate the summary statistics for a given list of numbers.

Here is a breakdown of how the code works:

* The `mean` function calculates the mean of a list of numbers by summing all the numbers in the list and dividing by the number of numbers in the list.
* The `standardDeviation` function calculates the standard deviation of a list of numbers by first calculating the mean of the list, then calculating the difference between each number in the list and the mean, then squaring each of those differences, then summing all the squared differences, and finally dividing the sum of the squared differences by the number of numbers in the list minus one.
* The `skewness` function calculates the skewness of a list of numbers by first calculating the mean and standard deviation of the list, then calculating the cube of the difference between each number in the list and the mean, then summing all the cubed differences, and finally dividing the sum of the cubed differences by the number of numbers in the list times the cube of the standard deviation.
* The `kurtosis` function calculates the kurtosis of a list of numbers by first calculating the mean and standard deviation of the list, then calculating the fourth power of the difference between each number in the list and the mean, then summing all the fourth powers, and finally dividing the sum of the fourth powers by the number of numbers in the list times the fourth power of the standard deviation.
* The `summaryStatistics` function takes a list of numbers as an argument and calculates the mean, standard deviation, skewness, and kurtosis of the list, then prints out the results.

The last few lines of the code define a list of numbers and then call the `summaryStatistics` function to calculate the summary statistics for the list.