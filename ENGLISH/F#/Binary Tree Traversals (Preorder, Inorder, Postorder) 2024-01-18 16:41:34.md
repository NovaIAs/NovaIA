```f#

// Define a type to represent a binary tree
type 'a Tree =
    | Leaf of 'a
    | Node of 'a Tree * 'a * 'a Tree

// Define a function to construct a binary tree from a list of values
let rec buildTree (values: 'a list): 'a Tree =
    match values with
    | [] -> Leaf null
    | [value] -> Leaf value
    | value :: rest -> Node (buildTree rest, value, buildTree (List.tail rest))

// Define a function to traverse a binary tree in preorder
let rec preorder (tree: 'a Tree): unit =
    match tree with
    | Leaf value -> printfn "%A" value
    | Node (left, value, right) ->
        printfn "%A" value
        preorder left
        preorder right

// Define a function to traverse a binary tree in inorder
let rec inorder (tree: 'a Tree): unit =
    match tree with
    | Leaf value -> printfn "%A" value
    | Node (left, value, right) ->
        inorder left
        printfn "%A" value
        inorder right

// Define a function to traverse a binary tree in postorder
let rec postorder (tree: 'a Tree): unit =
    match tree with
    | Leaf value -> printfn "%A" value
    | Node (left, value, right) ->
        postorder left
        postorder right
        printfn "%A" value

// Define some test data
let values = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

// Construct a binary tree from the test data
let tree = buildTree values

// Traverse the binary tree in preorder
printfn "Preorder:"
preorder tree
printfn ""

// Traverse the binary tree in inorder
printfn "Inorder:"
inorder tree
printfn ""

// Traverse the binary tree in postorder
printfn "Postorder:"
postorder tree
printfn ""

```

This code defines a type to represent a binary tree, a function to construct a binary tree from a list of values, and functions to traverse the binary tree in preorder, inorder, and postorder. The test data is a list of integers, and the output of the program is the tree traversed in the three different orders.