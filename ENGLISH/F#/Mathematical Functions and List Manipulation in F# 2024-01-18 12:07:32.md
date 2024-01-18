```f#
// Define a module for our program
module Program =

    // Define a function to calculate the factorial of a number
    let factorial n =
        if n <= 1 then 1
        else n * factorial (n - 1)

    // Define a function to calculate the Fibonacci sequence up to a given number
    let fibonacci n =
        if n <= 1 then n
        else fibonacci (n - 1) + fibonacci (n - 2)

    // Define a function to check if a number is prime
    let isPrime n =
        if n <= 1 then false
        else if n <= 3 then true
        else
            let rec isPrimeHelper m =
                if m = n then true
                else if m * m > n then true
                else if n % m = 0 then false
                else isPrimeHelper (m + 2)
            isPrimeHelper 3

    // Define a function to find all the prime numbers up to a given number
    let findPrimes n =
        let rec findPrimesHelper m primes =
            if m > n then primes
            else if isPrime m then findPrimesHelper (m + 1) (m :: primes)
            else findPrimesHelper (m + 1) primes
        findPrimesHelper 2 []

    // Define a function to calculate the greatest common divisor of two numbers
    let gcd a b =
        if b = 0 then a
        else gcd b (a % b)

    // Define a function to calculate the least common multiple of two numbers
    let lcm a b =
        a * b / gcd a b

    // Define a function to calculate the sum of all the numbers in a list
    let sum list =
        let rec sumHelper list sum =
            match list with
            | [] -> sum
            | h :: t -> sumHelper t (sum + h)
        sumHelper list 0

    // Define a function to calculate the average of all the numbers in a list
    let average list =
        sum list / List.length list

    // Define a function to find the maximum value in a list
    let max list =
        let rec maxHelper list max =
            match list with
            | [] -> max
            | h :: t -> maxHelper t (max h max)
        maxHelper list List.head list

    // Define a function to find the minimum value in a list
    let min list =
        let rec minHelper list min =
            match list with
            | [] -> min
            | h :: t -> minHelper t (min h min)
        minHelper list List.head list

    // Define a function to sort a list in ascending order
    let sort list =
        let rec sortHelper list sorted =
            match list with
            | [] -> sorted
            | h :: t -> sortHelper (List.filter (fun x -> x < h) list) (h :: sorted)
        sortHelper list []

    // Define a function to sort a list in descending order
    let sortDescending list =
        let rec sortDescendingHelper list sorted =
            match list with
            | [] -> sorted
            | h :: t -> sortDescendingHelper (List.filter (fun x -> x > h) list) (h :: sorted)
        sortDescendingHelper list []

    // Define a function to reverse a list
    let reverse list =
        let rec reverseHelper list reversed =
            match list with
            | [] -> reversed
            | h :: t -> reverseHelper t (h :: reversed)
        reverseHelper list []

    // Define a function to find the index of an element in a list
    let indexOf element list =
        let rec indexOfHelper element list index =
            match list with
            | [] -> -1
            | h :: t -> if h = element then index else indexOfHelper element t (index + 1)
        indexOfHelper element list 0

    // Define a function to remove an element from a list
    let remove element list =
        let rec removeHelper element list newList =
            match list with
            | [] -> newList
            | h :: t -> if h = element then removeHelper element t newList else removeHelper element t (h :: newList)
        removeHelper element list []

    // Define a function to insert an element into a list at a given index
    let insert element list index =
        let rec insertHelper element list index newList =
            match list with
            | [] -> if index = 0 then [element] else newList
            | h :: t -> if index = 0 then element :: list else insertHelper element t (index - 1) (h :: newList)
        insertHelper element list index []

    // Define a function to find all the subsequences of a list
    let subsequences list =
        let rec subsequencesHelper list subsequences =
            match list with
            | [] -> [[]]
            | h :: t -> subsequencesHelper t (subsequences @ (List.map (fun s -> h :: s) subsequences))
        subsequencesHelper list [[]]

    // Define a function to find all the subsets of a list
    let subsets list =
        let rec subsetsHelper list subsets =
            match list with
            | [] -> [[]]
            | h :: t -> subsetsHelper t (subsets @ (List.map (fun s -> h :: s) subsets))
        subsetsHelper list [[]]