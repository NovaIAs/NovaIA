```f#
// Create a type alias for a function that takes a list of integers and returns a list of integers
type IntListFunction = int list -> int list

// Define a function that takes a list of integers and returns the sum of the list
let sum (numbers: int list) =
    numbers |> List.fold (fun acc x -> acc + x) 0

// Define a function that takes a list of integers and returns the product of the list
let product (numbers: int list) =
    numbers |> List.fold (fun acc x -> acc * x) 1

// Define a function that takes a list of integers and returns the maximum value in the list
let max (numbers: int list) =
    numbers |> List.reduce (fun acc x -> if x > acc then x else acc)

// Define a function that takes a list of integers and returns the minimum value in the list
let min (numbers: int list) =
    numbers |> List.reduce (fun acc x -> if x < acc then x else acc)

// Define a function that takes a list of integers and returns a list of the prime numbers in the list
let primes (numbers: int list) =
    numbers |> List.filter (fun n -> isPrime n) where
        let isPrime (n: int) =
            if n < 2 then
                false
            else
                let sqrtN = int (sqrt (float n))
                [2 .. sqrtN] |> List.forall (fun i -> n % i <> 0)

// Define a function that takes a list of integers and returns a list of the even numbers in the list
let even (numbers: int list) =
    numbers |> List.filter (fun n -> n % 2 = 0)

// Define a function that takes a list of integers and returns a list of the odd numbers in the list
let odd (numbers: int list) =
    numbers |> List.filter (fun n -> n % 2 <> 0)

// Define a function that takes a list of integers and returns a list of the numbers that are divisible by 3 in the list
let divisibleByThree (numbers: int list) =
    numbers |> List.filter (fun n -> n % 3 = 0)

// Define a function that takes a list of integers and returns a list of the numbers that are not divisible by 3 in the list
let notDivisibleByThree (numbers: int list) =
    numbers |> List.filter (fun n -> n % 3 <> 0)

// Define a function that takes a list of integers and returns a list of the numbers that are greater than 5 in the list
let greaterThanFive (numbers: int list) =
    numbers |> List.filter (fun n -> n > 5)

// Define a function that takes a list of integers and returns a list of the numbers that are less than or equal to 5 in the list
let lessThanOrEqualToFive (numbers: int list) =
    numbers |> List.filter (fun n -> n <= 5)

// Define a function that takes a list of integers and returns a list of the numbers that are between 3 and 7 in the list
let betweenThreeAndSeven (numbers: int list) =
    numbers |> List.filter (fun n -> n > 3 && n < 7)

// Define a function that takes a list of integers and returns a list of the numbers that are not between 3 and 7 in the list
let notBetweenThreeAndSeven (numbers: int list) =
    numbers |> List.filter (fun n -> n <= 3 || n >= 7)
```

This F# code defines a series of functions that take a list of integers as input and return a new list of integers. The functions perform various operations on the input list, such as summing the numbers, finding the maximum and minimum values, filtering the list based on different criteria, and so on.

Here's a brief explanation of each function:

1. `sum`: This function returns the sum of all the numbers in the input list.

2. `product`: This function returns the product of all the numbers in the input list.

3. `max`: This function returns the maximum value in the input list.

4. `min`: This function returns the minimum value in the input list.

5. `primes`: This function returns a list of the prime numbers in the input list.

6. `even`: This function returns a list of the even numbers in the input list.

7. `odd`: This function returns a list of the odd numbers in the input list.

8. `divisibleByThree`: This function returns a list of the numbers in the input list that are divisible by 3.

9. `notDivisibleByThree`: This function returns a list of the numbers in the input list that are not divisible by 3.

10. `greaterThanFive`: This function returns a list of the numbers in the input list that are greater than 5.

11. `lessThanOrEqualToFive`: This function returns a list of the numbers in the input list that are less than or equal to 5.

12. `betweenThreeAndSeven`: This function returns a list of the numbers in the input list that are between 3 and 7.

13. `notBetweenThreeAndSeven`: This function returns a list of the numbers in the input list that are not between 3 and 7.