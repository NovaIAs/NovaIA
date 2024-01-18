```cool
-- Define a new data type called "Tree".
type Tree =
    -- A leaf node contains a single value.
    Leaf [Int]
|       -- A branch node contains a list of subtrees.
    Branch [Tree];

-- Define a function called "sumTree" that takes a tree as an argument and returns the sum of all the values in the tree.
def sumTree : Tree -> [Int] -> Int :=
    match tree with
        -- If the tree is a leaf, return the sum of its value.
        Leaf x => x.sum()
    |       -- If the tree is a branch, recursively sum the subtrees.
        Branch xs => xs.flatMap(sumTree).sum();

-- Define a function called "maxTree" that takes a tree as an argument and returns the maximum value in the tree.
def maxTree : Tree -> [Int] -> Int :=
    match tree with
        -- If the tree is a leaf, return its value.
        Leaf x => x.max()
    |       -- If the tree is a branch, recursively find the maximum value in the subtrees.
        Branch xs => xs.flatMap(maxTree).max();

-- Define a function called "minTree" that takes a tree as an argument and returns the minimum value in the tree.
def minTree : Tree -> [Int] -> Int :=
    match tree with
        -- If the tree is a leaf, return its value.
        Leaf x => x.min()
    |       -- If the tree is a branch, recursively find the minimum value in the subtrees.
        Branch xs => xs.flatMap(minTree).min();

-- Define a function called "averageTree" that takes a tree as an argument and returns the average of all the values in the tree.
def averageTree : Tree -> [Int] -> Int :=
    match tree with
        -- If the tree is a leaf, return its value.
        Leaf x => x.sum() / x.size()
    |       -- If the tree is a branch, recursively find the average of the subtrees.
        Branch xs => xs.flatMap(averageTree).sum() / xs.flatMap(size).sum();

-- Define a function called "sizeTree" that takes a tree as an argument and returns the number of nodes in the tree.
def sizeTree : Tree -> [Int] -> Int :=
    match tree with
        -- If the tree is a leaf, return 1.
        Leaf x => 1
    |       -- If the tree is a branch, recursively count the nodes in the subtrees.
        Branch xs => 1 + xs.flatMap(sizeTree).sum();

-- Define a function called "heightTree" that takes a tree as an argument and returns the height of the tree.
def heightTree : Tree -> [Int] -> Int :=
    match tree with
        -- If the tree is a leaf, return 1.
        Leaf x => 1
    |       -- If the tree is a branch, recursively find the maximum height of the subtrees.
        Branch xs => 1 + xs.flatMap(heightTree).max();

-- Define a function called "isBalancedTree" that takes a tree as an argument and returns true if the tree is balanced, and false otherwise.
def isBalancedTree : Tree -> [Int] -> Bool :=
    match tree with
        -- If the tree is a leaf, it is balanced.
        Leaf x => true
    |       -- If the tree is a branch, check if the subtrees are balanced and have similar heights.
        Branch xs => xs.forall(isBalancedTree) and xs.flatMap(heightTree).max() - xs.flatMap(heightTree).min() <= 1;

-- Define a function called "printTree" that takes a tree as an argument and prints it to the console.
def printTree : Tree -> [Int] -> Unit :=
    match tree with
        -- If the tree is a leaf, print its value.
        Leaf x => println(x.toString())
    |       -- If the tree is a branch, recursively print the subtrees.
        Branch xs => xs.forEach(printTree);

-- Create a new tree.
let tree = Branch [Leaf [1, 2, 3], Leaf [4, 5, 6], Branch [Leaf [7, 8, 9], Leaf [10, 11, 12]]];

-- Print the tree.
println("Tree:");
printTree(tree);

-- Print the sum of the values in the tree.
println("\nSum of values:");
println(sumTree(tree));

-- Print the maximum value in the tree.
println("\nMaximum value:");
println(maxTree(tree));

-- Print the minimum value in the tree.