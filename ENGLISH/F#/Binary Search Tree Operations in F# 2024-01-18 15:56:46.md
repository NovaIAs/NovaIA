```f#

// Define a type to represent a binary search tree.
type BinarySearchTree<'a> =
  | Empty
  | Node of 'a * BinarySearchTree<'a> * BinarySearchTree<'a>

// Define a helper function to insert a value into a binary search tree.
let rec insert<'a> (value : 'a) (tree : BinarySearchTree<'a>) : BinarySearchTree<'a> =
  match tree with
  | Empty -> Node(value, Empty, Empty)
  | Node(v, left, right) ->
    if value < v then Node(v, insert value left, right)
    else Node(v, left, insert value right)

// Define a helper function to search for a value in a binary search tree.
let rec search<'a> (value : 'a) (tree : BinarySearchTree<'a>) : bool =
  match tree with
  | Empty -> false
  | Node(v, left, right) ->
    if value = v then true
    elif value < v then search value left
    else search value right

// Define a helper function to delete a value from a binary search tree.
let rec delete<'a> (value : 'a) (tree : BinarySearchTree<'a>) : BinarySearchTree<'a> =
  match tree with
  | Empty -> Empty
  | Node(v, left, right) ->
    if value < v then Node(v, delete value left, right)
    elif value > v then Node(v, left, delete value right)
    else
      match left, right with
      | Empty, Empty -> Empty
      | Empty, _ -> right
      | _, Empty -> left
      | _, _ ->
        let minValue = findMin right
        Node(minValue, left, delete minValue right)

// Define a helper function to find the minimum value in a binary search tree.
let rec findMin<'a> (tree : BinarySearchTree<'a>) : 'a =
  match tree with
  | Empty -> failwith "Cannot find the minimum value of an empty tree."
  | Node(v, Empty, _) -> v
  | Node(v, left, _) -> findMin left

// Define a function to create a binary search tree from an array of values.
let create<'a> (values : 'a[]) : BinarySearchTree<'a> =
  List.fold (fun tree value -> insert value tree) Empty values

// Define a function to print the values of a binary search tree in sorted order.
let rec print<'a> (tree : BinarySearchTree<'a>) =
  match tree with
  | Empty -> ()
  | Node(v, left, right) ->
    print left;
    printfn "%A" v;
    print right

// Create a binary search tree from an array of values.
let tree = create [1; 3; 5; 7; 9; 11; 13; 15; 17; 19]

// Print the values of the binary search tree in sorted order.
print tree

// Search for a value in the binary search tree.
let found = search 11 tree

// Print the result of the search.
printfn "Found the value: %b" found

// Delete a value from the binary search tree.
let newTree = delete 7 tree

// Print the values of the new binary search tree in sorted order.
print newTree
```

### Explanation:

The provided code is a complex F# program that demonstrates various operations on a binary search tree. Below is a detailed explanation of the code:

1. **BinarySearchTree Type:**
   - The `BinarySearchTree<'a>` type represents a binary search tree. It can be either `Empty` (indicating an empty tree) or a `Node` containing a value, a left subtree, and a right subtree.

2. **Insertion Function (`insert`):**
   - The `insert` function recursively inserts a value into a binary search tree. It traverses the tree based on the values and inserts the new value as a leaf node in the appropriate location to maintain the binary search tree property.

3. **Search Function (`search`):**
   - The `search` function recursively searches for a value in a binary search tree. It traverses the tree based on the values and returns `true` if the value is found in the tree, and `false` otherwise.

4. **Deletion Function (`delete`):**
   - The `delete` function recursively deletes a value from a binary search tree. It handles various cases such as deleting a node with no children, one child, or two children, while maintaining the binary search tree property.

5. **Finding the Minimum Value (`findMin`):**
   - The `findMin` function recursively finds the minimum value in a binary search tree. It traverses the tree to find the leftmost node, which contains the minimum value in a binary search tree.

6. **Creating a Binary Search Tree (`create`):**
   - The `create` function creates a binary search tree from an array of values. It uses `List.fold` to insert each value into an empty tree, resulting in a balanced binary search tree.

7. **Printing the Binary Search Tree (`print`):**
   - The `print` function recursively prints the values of a binary search tree in sorted order. It uses an in-order traversal to print the values in ascending order.

8. **Usage of the Functions:**
   - The code creates a binary search tree from an array of values, prints the values in sorted order, searches for a value in the tree, and deletes a value from the tree, demonstrating the functionality of the binary search tree operations.

This code provides a comprehensive example of working with binary search trees in F#, including insertion, searching, deletion, and printing operations. It demonstrates the use of recursive functions and showcases the power of F# for implementing complex data structures and algorithms.