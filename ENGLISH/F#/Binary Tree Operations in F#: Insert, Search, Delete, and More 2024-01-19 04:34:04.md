```f#
// Define a type to represent a binary tree node
type BinaryTreeNode<'a> =
    | Leaf of 'a
    | Node of 'a * BinaryTreeNode<'a> * BinaryTreeNode<'a>

// Define a function to insert a value into a binary tree
let insert<'a> (value: 'a) (tree: BinaryTreeNode<'a>) : BinaryTreeNode<'a> =
    match tree with
    | Leaf _ -> Node(value, Leaf(value), Leaf(value))
    | Node(v, left, right) ->
        if v < value then Node(v, left, insert value right)
        else Node(v, insert value left, right)

// Define a function to search for a value in a binary tree
let search<'a> (value: 'a) (tree: BinaryTreeNode<'a>) : bool =
    match tree with
    | Leaf _ -> false
    | Node(v, left, right) ->
        if v = value then true
        elif v < value then search value right
        else search value left

// Define a function to delete a value from a binary tree
let delete<'a> (value: 'a) (tree: BinaryTreeNode<'a>) : BinaryTreeNode<'a> =
    match tree with
    | Leaf _ -> Leaf value
    | Node(v, left, right) ->
        if v = value then
            match left, right with
            | Leaf _, Leaf _ -> Leaf value
            | Leaf _, Node(_, _, _) -> right
            | Node(_, _, _), Leaf _ -> left
            | Node(_, left2, right2) ->
                let minValue = getMinValue right2
                Node(minValue, left, delete minValue right2)
        elif v < value then Node(v, left, delete value right)
        else Node(v, delete value left, right)

// Define a function to get the minimum value in a binary tree
let getMinValue<'a> (tree: BinaryTreeNode<'a>) : 'a =
    match tree with
    | Leaf v -> v
    | Node(v, left, _) -> getMinValue left

// Define a function to get the maximum value in a binary tree
let getMaxValue<'a> (tree: BinaryTreeNode<'a>) : 'a =
    match tree with
    | Leaf v -> v
    | Node(v, _, right) -> getMaxValue right

// Define a function to check if a binary tree is balanced
let isBalanced<'a> (tree: BinaryTreeNode<'a>) : bool =
    match tree with
    | Leaf _ -> true
    | Node(_, left, right) ->
        let leftHeight = height left
        let rightHeight = height right
        abs(leftHeight - rightHeight) <= 1 && isBalanced left && isBalanced right

// Define a function to get the height of a binary tree
let height<'a> (tree: BinaryTreeNode<'a>) : int =
    match tree with
    | Leaf _ -> 0
    | Node(_, left, right) -> 1 + max (height left) (height right)

// Define a function to print a binary tree
let printTree<'a> (tree: BinaryTreeNode<'a>) =
    match tree with
    | Leaf v -> printfn "%d" v
    | Node(v, left, right) ->
        printfn "%d" v
        printTree left
        printTree right

// Define a function to test the binary tree functions
let testBinaryTree() =
    let tree = insert 10 (insert 5 (insert 20 (insert 7 (Leaf 1)))))
    printfn "Insert: %A" tree
    printfn "Search: %b" (search 7 tree)
    printfn "Delete: %A" (delete 7 tree)
    printfn "Get minimum value: %d" (getMinValue tree)
    printfn "Get maximum value: %d" (getMaxValue tree)
    printfn "Is balanced: %b" (isBalanced tree)
    printfn "Height: %d" (height tree)
    printfn "Print tree:"
    printTree tree

// Call the test function
testBinaryTree()
```

This code defines a binary tree data structure in F# and provides various functions to manipulate and query the tree. Here's an explanation of the code:

1. `BinaryTreeNode<'a>`: This type represents a binary tree node. It can be either a `Leaf` containing a value or a `Node` consisting of a value and two child nodes.

2. `insert`: This function inserts a value into a binary tree. It recursively traverses the tree and inserts the value in the appropriate position based on the value of the current node.

3. `search`: This function searches for a value in a binary tree. It recursively traverses the tree and checks if the current node's value matches the search value.

4. `delete`: This function deletes a value from a binary tree. It recursively traverses the tree and removes the node containing the value. It handles various cases, including nodes with no children, one child, and two children.

5. `getMinValue`: This function gets the minimum value in a binary tree. It recursively traverses the tree and finds the node with the smallest value.

6. `getMaxValue`: This function gets the maximum value in a binary tree. It recursively traverses the tree and finds the node with the largest value.

7. `isBalanced`: This function checks if a binary tree is balanced. A binary tree is balanced if the height of its left and right subtrees differs by at most 1.

8. `height`: This function gets the height of a binary tree. The height of a tree is the maximum number of nodes along the longest path from the root node to a leaf node.

9. `printTree`: This function prints the values of the nodes in a binary tree in order.

10. `testBinaryTree`: This function creates a binary tree, performs various operations on it (insert, search, delete, etc.), and prints the results.

When you call `testBinaryTree()`, it creates a binary tree with values 10, 5, 20, 7, and 1. It then performs various operations on the tree and prints the results. This code demonstrates the functionality of the binary tree data structure and the various operations that can be performed on it.