```f#
// Define a type to represent a binary tree node.
type Node<T> =
    | Leaf of T
    | Branch of Node<T> * Node<T>

// Define a function to construct a binary tree from a list of values.
let rec buildTree xs =
    match xs with
    | [] -> Leaf 0
    | x::xs -> Branch (buildTree xs, buildTree (tail xs))

// Define a function to calculate the sum of all values in a binary tree.
let rec sumTree t =
    match t with
    | Leaf x -> x
    | Branch (l, r) -> sumTree l + sumTree r

// Define a function to calculate the maximum value in a binary tree.
let rec maxTree t =
    match t with
    | Leaf x -> x
    | Branch (l, r) -> max (maxTree l) (maxTree r)

// Define a function to calculate the minimum value in a binary tree.
let rec minTree t =
    match t with
    | Leaf x -> x
    | Branch (l, r) -> min (minTree l) (minTree r)

// Define a function to calculate the average value in a binary tree.
let rec avgTree t =
    match t with
    | Leaf x -> x
    | Branch (l, r) -> (sumTree t) / (countNodes t)

// Define a function to calculate the number of nodes in a binary tree.
let rec countNodes t =
    match t with
    | Leaf _ -> 1
    | Branch (l, r) -> countNodes l + countNodes r

// Define a function to print a binary tree in a human-readable format.
let rec printTree t =
    match t with
    | Leaf x -> printfn "%d" x
    | Branch (l, r) -> printfn "Branch(%d, %d)" (printTree l) (printTree r)

// Create a binary tree from a list of values.
let tree = buildTree [1, 2, 3, 4, 5, 6, 7]

// Print the binary tree in a human-readable format.
printfn "Tree:\n%s" (printTree tree)

// Calculate the sum of all values in the binary tree.
let sum = sumTree tree
printfn "Sum: %d" sum

// Calculate the maximum value in the binary tree.
let max = maxTree tree
printfn "Max: %d" max

// Calculate the minimum value in the binary tree.
let min = minTree tree
printfn "Min: %d" min

// Calculate the average value in the binary tree.
let avg = avgTree tree
printfn "Avg: %f" avg

// Calculate the number of nodes in the binary tree.
let count = countNodes tree
printfn "Count: %d" count
```

This code performs a variety of operations on a binary tree, including:

- Constructing a binary tree from a list of values
- Calculating the sum of all values in the binary tree
- Calculating the maximum value in the binary tree
- Calculating the minimum value in the binary tree
- Calculating the average value in the binary tree
- Calculating the number of nodes in the binary tree
- Printing the binary tree in a human-readable format

The code is well-documented and uses a variety of F# features, including pattern matching, recursion, and the `printfn` function for formatted output.