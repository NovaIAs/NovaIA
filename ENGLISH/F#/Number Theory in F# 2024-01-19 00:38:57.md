```f#

// Define a function that computes the factorial of a non-negative integer
let factorial n =
    if n < 0 then failwith "factorial is undefined for negative integers"
    else if n = 0 then 1
    else n * factorial (n - 1)

// Define a function that computes the sum of a list of numbers
let sum lst =
    List.fold (+) 0 lst

// Define a function that computes the product of a list of numbers
let product lst =
    List.fold (*) 1 lst

// Define a function that takes two lists of numbers and returns a list of pairs of numbers, where the first number in each pair is from the first list and the second number in each pair is from the second list
let zipWith (f : int * int -> 'a) (lst1 : int list) (lst2 : int list) =
    if lst1.Length <> lst2.Length then failwith "lists must have the same length"
    else
        let rec zipWithAux acc i =
            if i >= lst1.Length then acc
            else zipWithAux (acc @ [f (lst1.[i]) (lst2.[i])]) (i + 1)
        zipWithAux [] 0

// Define a function that takes a list of numbers and returns a list of tuples, where each tuple contains the number and its index in the list
let enumerate lst =
    List.mapi (fun i x -> (x, i)) lst

// Define a function that takes a list of numbers and returns a list of the numbers reversed
let reverse lst =
    List.fold (fun acc x -> x :: acc) [] lst

// Define a function that takes a list of numbers and returns a list of the numbers sorted in ascending order
let sortAscending lst =
    List.sort (fun x y -> x - y) lst

// Define a function that takes a list of numbers and returns a list of the numbers sorted in descending order
let sortDescending lst =
    List.sort (fun x y -> y - x) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are even
let filterEven lst =
    List.filter (fun x -> x % 2 = 0) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are odd
let filterOdd lst =
    List.filter (fun x -> x % 2 <> 0) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are greater than a given number
let filterGreaterThan n lst =
    List.filter (fun x -> x > n) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are less than a given number
let filterLessThan n lst =
    List.filter (fun x -> x < n) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are between two given numbers
let filterBetween (n1, n2) lst =
    List.filter (fun x -> n1 < x && x < n2) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are multiples of a given number
let filterMultipleOf n lst =
    List.filter (fun x -> x % n = 0) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are not multiples of a given number
let filterNotMultipleOf n lst =
    List.filter (fun x -> x % n <> 0) lst

// Define a function that takes a list of numbers and returns a list of the numbers that are prime numbers
let filterPrime lst =
    List.filter (fun x -> isPrime x) lst
    where isPrime n =
        if n < 2 then false
        else if n = 2 then true
        else
            let rec isPrimeAux i =
                if i * i > n then true
                else if n % i = 0 then false
                else isPrimeAux (i + 2)
            isPrimeAux 3

// Define a function that takes a list of numbers and returns a list of the numbers that are composite numbers
let filterComposite lst =
    List.filter (fun x -> not (isPrime x)) lst
    where isPrime n =
        if n < 2 then false
        else if n = 2 then true
        else
            let rec isPrimeAux i =
                if i * i > n then true
                else if n % i = 0 then false
                else isPrimeAux (i + 2)
            isPrimeAux 3

// Define a function that takes a list of numbers and