// This F# program demonstrates complex logic with multiple modules.

module MatrixOperations =
    // Function to add two matrices
    let add (m1: int[][]) (m2: int[][])  =
        Array.map2 (fun a b -> a + b) m1 m2

    // Function to multiply two matrices
    let multiply (m1: int[][]) (m2: int[][])  =
        Array.init (Array.length m1) (fun i ->
            Array.init (Array.length m2.[0]) (fun j ->
                Array.sum (Array.map2 (fun a b -> a * b) m1.[i] m2.|[j])))

module StringOperations =
    // Function to reverse a string
    let reverse (s: string)  =
        Array.foldBack (fun c acc -> c + acc) s ""

    // Function to find the longest common substring between two strings
    let longestCommonSubstring (s1: string) (s2: string)  =
        let rec lcsHelper i j =
            if i = 0 || j = 0 then ""
            else if s1.[i-1] = s2.[j-1] then lcsHelper (i-1) (j-1) + s1.[i-1]
            else max (lcsHelper (i-1) j) (lcsHelper i (j-1))
        lcsHelper (String.length s1) (String.length s2)

module DataStructures =
    // Define a node for a binary search tree
    type 'a BSTNode =
        | Leaf
        | Node of 'a * 'a BSTNode * 'a BSTNode

    // Function to insert a value into a binary search tree
    let insert (value: int) (tree: int BSTNode)  =
        match tree with
        | Leaf -> Node(value, Leaf, Leaf)
        | Node(v, left, right) ->
            if value < v then Node(v, insert value left, right)
            else Node(v, left, insert value right)

    // Function to find a value in a binary search tree
    let find (value: int) (tree: int BSTNode)  =
        match tree with
        | Leaf -> false
        | Node(v, left, right) ->
            if value = v then true
            else if value < v then find value left
            else find value right

// Main program
let main argv =
    // Create two matrices for matrix operations
    let matrix1 = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]
    let matrix2 = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]

    // Add and multiply the matrices
    let resultMatrix = MatrixOperations.add matrix1 matrix2
    let productMatrix = MatrixOperations.multiply matrix1 matrix2

    // Print the original matrices and their sum and product
    printfn "Original matrices:"
    printfn "%A" matrix1
    printfn "%A" matrix2
    printfn "Sum of matrices:"
    printfn "%A" resultMatrix
    printfn "Product of matrices:"
    printfn "%A" productMatrix

    // Reverse two strings for string operations
    let string1 = "Hello, world!"
    let string2 = "Reverse this string"

    // Print the original strings and their reversed and longest common substring versions
    printfn "Original strings:"
    printfn "%s" string1
    printfn "%s" string2
    printfn "Reversed strings:"
    printfn "%s" (StringOperations.reverse string1)
    printfn "%s" (StringOperations.reverse string2)
    printfn "Longest common substring:"
    printfn "%s" (StringOperations.longestCommonSubstring string1 string2)

    // Create a binary search tree for data structures
    let tree = DataStructures.insert 10 Leaf
    tree |> DataStructures.insert 5 |> DataStructures.insert 15 |> DataStructures.insert 2 |> DataStructures.insert 7 |> DataStructures.insert 12

    // Search for a value in the binary search tree
    if DataStructures.find 7 tree then
        printfn "Value found in the tree"
    else
        printfn "Value not found in the tree"

    0  // Return 0 to indicate successful execution

// Entry point
[<EntryPoint>]
let main argv =
    try
        main argv
    with
    | exn ->
        printfn "An exception occurred: %A" exn
        1  // Return 1 to indicate an error