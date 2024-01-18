```f#
// Define a type to represent a binary tree node
type BinaryTreeNode<'T> =
    | Leaf of 'T
    | Node of 'T * BinaryTreeNode<'T> * BinaryTreeNode<'T>

// Define a function to create a binary tree from a list of values
let createBinaryTree (values: 'T list) : BinaryTreeNode<'T> =
    match values with
    | [] -> Leaf null
    | value :: values ->
        let left = createBinaryTree values
        let right = createBinaryTree (List.tail values)
        Node(value, left, right)

// Define a function to find the maximum value in a binary tree
let findMaxValue (tree: BinaryTreeNode<'T>) : 'T =
    match tree with
    | Leaf value -> value
    | Node(value, left, right) ->
        let leftMax = findMaxValue left
        let rightMax = findMaxValue right
        [|value; leftMax; rightMax|] |> List.max

// Define a function to find the minimum value in a binary tree
let findMinValue (tree: BinaryTreeNode<'T>) : 'T =
    match tree with
    | Leaf value -> value
    | Node(value, left, right) ->
        let leftMin = findMinValue left
        let rightMin = findMinValue right
        [|value; leftMin; rightMin|] |> List.min

// Define a function to find the height of a binary tree
let findHeight (tree: BinaryTreeNode<'T>) : int =
    match tree with
    | Leaf _ -> 0
    | Node(_, left, right) ->
        1 + max (findHeight left) (findHeight right)

// Define a function to check if a binary tree is balanced
let isBalanced (tree: BinaryTreeNode<'T>) : bool =
    match tree with
    | Leaf _ -> true
    | Node(_, left, right) ->
        let leftHeight = findHeight left
        let rightHeight = findHeight right
        abs (leftHeight - rightHeight) <= 1 &&
        isBalanced left &&
        isBalanced right

// Define a function to print the values of a binary tree in preorder traversal
let printPreorder (tree: BinaryTreeNode<'T>) : unit =
    match tree with
    | Leaf value ->
        printfn "%A" value
    | Node(value, left, right) ->
        printfn "%A" value
        printPreorder left
        printPreorder right

// Define a function to print the values of a binary tree in inorder traversal
let printInorder (tree: BinaryTreeNode<'T>) : unit =
    match tree with
    | Leaf value ->
        printfn "%A" value
    | Node(value, left, right) ->
        printInorder left
        printfn "%A" value
        printInorder right

// Define a function to print the values of a binary tree in postorder traversal
let printPostorder (tree: BinaryTreeNode<'T>) : unit =
    match tree with
    | Leaf value ->
        printfn "%A" value
    | Node(value, left, right) ->
        printPostorder left
        printPostorder right
        printfn "%A" value

// Define a driver function to test the binary tree functions
let main () =
    let values = [1; 2; 3; 4; 5; 6; 7]
    let tree = createBinaryTree values
    printfn "Maximum value: %A" (findMaxValue tree)
    printfn "Minimum value: %A" (findMinValue tree)
    printfn "Height: %i" (findHeight tree)
    printfn "Is balanced: %b" (isBalanced tree)
    printfn "Preorder traversal:"
    printPreorder tree
    printfn "Inorder traversal:"
    printInorder tree
    printfn "Postorder traversal:"
    printPostorder tree

// Call the driver function
main ()
```