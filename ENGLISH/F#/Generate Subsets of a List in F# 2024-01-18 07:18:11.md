```f#
// Create a function to generate all subsets of a list
let rec subsets (xs : list<int>) : list<list<int>> =
    match xs with
    | [] -> [[]]  // The subsets of the empty list is just the empty list itself
    | x::xs ->
        // Recursively compute the subsets of the tail of the list
        let ss = subsets xs
        // For each subset of the tail, prepend the head of the list to create a new subset
        ss @ (List.map (fun s -> x::s) ss)

// Define a list of numbers to generate subsets for
let numbers = [1; 2; 3; 4; 5]

// Generate all subsets of the list
let allSubsets = subsets numbers

// Print the subsets to the console
printfn "All subsets of %A:" numbers
for subset in allSubsets do
    printfn "%A" subset
```

Explanation:

1. We start by defining a recursive function called `subsets` that takes a list of integers as an argument and returns a list of all subsets of that list.
2. The function uses pattern matching to handle two cases:

    - If the input list is empty, it returns a list containing only the empty list itself. This is because the subsets of the empty list are just the empty list itself.
    - If the input list is not empty, it recursively computes the subsets of the tail of the list using the `subsets` function. Then, for each subset of the tail, it prepends the head of the list to create a new subset.

3. We then define a list of numbers called `numbers` that we want to generate subsets for.
4. We call the `subsets` function with the `numbers` list as an argument to compute all subsets of the list.
5. Finally, we use a `for` loop to iterate over the list of subsets and print each subset to the console.

This code demonstrates a recursive function that can be used to generate all subsets of a list. It uses a combination of pattern matching and list manipulation to efficiently compute all possible subsets of a given list.