```f#
// Define a type to represent a binary tree node
type BinaryTreeNode<'T> =
    | Leaf of 'T
    | Node of 'T * BinaryTreeNode<'T> * BinaryTreeNode<'T>

// Define a function to insert a value into a binary tree
let insert (value: 'T) (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf _ -> Node(value, Leaf value, Leaf value)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insert value left, right)
        else
            Node(v, left, insert value right)

// Define a function to search for a value in a binary tree
let search (value: 'T) (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf _ -> false
    | Node(v, left, right) ->
        if value = v then
            true
        elif value < v then
            search value left
        else
            search value right

// Define a function to delete a value from a binary tree
let delete (value: 'T) (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf _ -> Leaf value
    | Node(v, left, right) ->
        if value = v then
            match left, right with
            | Leaf _, Leaf _ -> Leaf value
            | Leaf _, Node(v, _, _) -> Node(v, left, right)
            | Node(v, _, _), Leaf _ -> Node(v, left, right)
            | Node(v, left, right) ->
                let min = findMin right
                Node(min, left, delete min right)
        elif value < v then
            Node(v, delete value left, right)
        else
            Node(v, left, delete value right)

// Define a function to find the minimum value in a binary tree
let findMin (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf v -> v
    | Node(v, left, _) -> findMin left

// Define a function to find the maximum value in a binary tree
let findMax (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf v -> v
    | Node(v, _, right) -> findMax right

// Define a function to print the values of a binary tree in order
let printInOrder (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf v -> printfn "%A " v
    | Node(v, left, right) ->
        printInOrder left
        printfn "%A " v
        printInOrder right

// Define a function to print the values of a binary tree in preorder
let printPreOrder (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf v -> printfn "%A " v
    | Node(v, left, right) ->
        printfn "%A " v
        printPreOrder left
        printPreOrder right

// Define a function to print the values of a binary tree in postorder
let printPostOrder (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf v -> printfn "%A " v
    | Node(v, left, right) ->
        printPostOrder left
        printPostOrder right
        printfn "%A " v

// Define a function to create a balanced binary tree from an array of values
let createBalancedTree (values: 'T[]) =
    let rec createBalancedTreeHelper (values: 'T[]) =
        match values with
        | [] -> Leaf value
        | [value] -> Leaf value
        | values ->
            let mid = values.Length / 2
            Node(values.[mid], createBalancedTreeHelper (Array.sub values 0 mid), createBalancedTreeHelper (Array.sub values (mid + 1) (values.Length - mid - 1)))
    createBalancedTreeHelper values

// Define a function to check if a binary tree is balanced
let isBalanced (tree: BinaryTreeNode<'T>) =
    let rec isBalancedHelper (tree: BinaryTreeNode<'T>) (depth: int) =
        match tree with
        | Leaf _ -> (depth, true)
        | Node(v, left, right) ->
            let leftDepth, leftBalanced = isBalancedHelper left (depth + 1)
            let rightDepth, rightBalanced = isBalancedHelper right (depth + 1)
            let balanced = leftBalanced && rightBalanced && abs(leftDepth - rightDepth) <= 1
            (max leftDepth rightDepth, balanced)
    let depth, balanced = isBalancedHelper tree 0
    balanced

// Define a function to count the number of nodes in a binary tree
let countNodes (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf _ -> 1
    | Node(v, left, right) -> 1 + countNodes left + countNodes right

// Define a function to find the height of a binary tree
let height (tree: BinaryTreeNode<'T>) =
    match tree with
    | Leaf _ -> 0
    | Node(v, left, right) -> 1 + max (height left) (height right)

// Define a function to check if a binary tree is a complete binary tree
let isComplete (tree: BinaryTreeNode<'T>) =
    let rec isCompleteHelper (tree: BinaryTreeNode<'T>) (index: int) (size: int) =
        match tree with
        | Leaf _ -> index = size
        | Node(v, left, right) ->
            isCompleteHelper left (2 * index + 1) size && isCompleteHelper right (2 * index + 2) size
    isCompleteHelper tree 0 (countNodes tree)

// Define a function to check if a binary tree is a perfect binary tree
let isPerfect (tree: BinaryTreeNode<'T>) =
    let height = height tree
    let size = countNodes tree
    let perfect = height = log2 size
    perfect

// Define a function to check if a binary tree is a full binary tree
let isFull (tree: BinaryTreeNode<'T>) =
    let height = height tree
    let size = countNodes tree
    let full = height = log2 (size + 1)
    full
```

This code implements a binary tree data structure in F#. A binary tree is a data structure that consists of nodes, where each node can have at most two children. The code includes functions for inserting, searching, and deleting values from the tree, as well as functions for printing the values of the tree in different orders. The code also includes functions for creating balanced binary trees, checking if a tree is balanced, counting the number of nodes in a tree, finding the height of a tree, and checking if a tree is complete, perfect, or full.

The code is well-commented and easy to understand. It uses the `match` expression to handle the different cases of the binary tree nodes. The code also uses recursion to implement some of the functions, such as the `insert`, `search`, and `delete` functions.