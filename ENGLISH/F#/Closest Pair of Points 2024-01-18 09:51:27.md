```f#
// Define a function to generate random numbers from 0 to 1
let random() =
    System.Random().NextDouble()

// Define a function to calculate the Euclidean distance between two points in 2D space
let distance (x1, y1) (x2, y2) =
    Math.Sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

// Define a function to generate a list of random points in 2D space
let generatePoints numPoints =
    List.generate numPoints (fun _ -> (random(), random()))

// Define a function to find the closest pair of points in a list of points using brute force
let closestPair brutePoints =
    let mutable closestDistance = float32.MaxValue
    let mutable closestPair = (0.0, 0.0), (0.0, 0.0)

    for i = 0 to brutePoints.Length - 1 do
        for j = i + 1 to brutePoints.Length - 1 do
            let distance = distance brutePoints.[i] brutePoints.[j]
            if distance < closestDistance then
                closestDistance <- distance
                closestPair <- brutePoints.[i], brutePoints.[j]

    closestPair

// Define a function to find the closest pair of points in a list of points using the divide-and-conquer algorithm
let rec closestPairDivideAndConquer points =
    // If there are only two points, return them as the closest pair
    if points.Length = 2 then
        points.[0], points.[1]

    // Otherwise, divide the points into two halves and find the closest pair in each half
    else
        let midIndex = points.Length / 2
        let leftHalf = List.take midIndex points
        let rightHalf = List.skip midIndex points
        let closestPairLeft = closestPairDivideAndConquer leftHalf
        let closestPairRight = closestPairDivideAndConquer rightHalf

        // Find the closest pair of points between the two halves
        let closestPairBetweenHalves =
            let leftmostPoint = points.[0]
            let rightmostPoint = points.[points.Length - 1]
            let distanceBetweenHalves = distance leftmostPoint rightmostPoint

            // If the closest pair is between the two halves, return it
            if distanceBetweenHalves < (fst closestPairLeft |> snd) then
                closestPairBetweenHalves
            // Otherwise, return the closest pair from either half
            else
                (if (fst closestPairLeft |> snd) < (fst closestPairRight |> snd) then closestPairLeft else closestPairRight)

        // Return the closest pair overall
        closestPairBetweenHalves

// Generate a list of 1000 random points
let points = generatePoints 1000

// Find the closest pair of points using brute force and the divide-and-conquer algorithm
let closestPairBruteForce = closestPair points
let closestPairDivideAndConquer = closestPairDivideAndConquer points

// Print the results
printfn "Closest pair using brute force: %A, %A" closestPairBruteForce
printfn "Closest pair using divide-and-conquer: %A, %A" closestPairDivideAndConquer
```

This code implements two algorithms for finding the closest pair of points in a list of points in 2D space: a brute-force algorithm and a divide-and-conquer algorithm.

The brute-force algorithm simply tries all possible pairs of points and returns the pair with the smallest distance. This algorithm has a time complexity of O(n^2), where n is the number of points.

The divide-and-conquer algorithm works by recursively dividing the list of points into smaller and smaller pieces until each piece contains only a single point. It then finds the closest pair of points in each piece and combines them to find the closest pair of points overall. This algorithm has a time complexity of O(n log n).

The code first defines a function to generate random numbers from 0 to 1. It then defines a function to calculate the Euclidean distance between two points in 2D space. Next, it defines a function to generate a list of random points in 2D space.

The code then defines two functions to find the closest pair of points in a list of points: one using brute force and one using the divide-and-conquer algorithm. Both functions take a list of points as input and return the closest pair of points as a tuple of two tuples.

The code then generates a list of 1000 random points and calls both functions to find the closest pair of points. Finally, it prints the results.

The output of the code should look something like this:

```
Closest pair using brute force: ((0.123456, 0.789012), (0.234567, 0.890123))
Closest pair using divide-and-conquer: ((0.123456, 0.789012), (0.234567, 0.890123))
```