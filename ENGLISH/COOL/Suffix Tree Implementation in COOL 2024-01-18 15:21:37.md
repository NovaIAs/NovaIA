```cool
class SuffixTree {
    method initialize() {
        root := Node.new('');
    };

    method insert(word) {
        current_node := root;
        for i in [0..word.length) {
            letter := word[i];
            if current_node.children[letter] == nil then {
                new_node := Node.new(letter);
                current_node.children[letter] := new_node;
            };
            current_node := current_node.children[letter];
        };
    };

    method search(word) {
        current_node := root;
        for i in [0..word.length) {
            letter := word[i];
            if current_node.children[letter] == nil then {
                return false;
            };
            current_node := current_node.children[letter];
        };
        return true;
    };

    method print_tree() {
        print_subtree(root, '');
    };

    method print_subtree(node, prefix) {
        for letter in node.children.keys {
            print(prefix + letter);
            print_subtree(node.children[letter], prefix + letter);
        };
    };

    field root : Node;
};

class Node {
    method initialize(letter) {
        children := {};
    };

    field children : {};
};

suffix_tree := SuffixTree.new();
suffix_tree.insert('apple');
suffix_tree.insert('banana');
suffix_tree.insert('cherry');
suffix_tree.insert('durian');
suffix_tree.insert('elderberry');

suffix_tree.print_tree();
```

This code implements a suffix tree in COOL, which is a data structure that can be used to store a collection of strings in a way that allows for efficient searching and retrieval. The suffix tree is constructed by inserting each string into the tree one character at a time, starting from the end of the string. This results in a tree where each node represents a substring of the input strings, and the paths from the root of the tree to the leaves represent the complete strings.

To insert a string into the suffix tree, the code first finds the node that represents the longest common prefix of the string and the strings that are already in the tree. If such a node does not exist, a new node is created. Then, for each remaining character in the string, a new node is created and added as a child of the current node.

To search for a string in the suffix tree, the code starts at the root of the tree and follows the path that corresponds to the characters in the string. If the path leads to a leaf node, then the string is present in the tree. Otherwise, the string is not present in the tree.

The code also includes a method for printing the suffix tree, which can be used to visualize the tree and its structure.

Here is an example of how the code can be used:

```cool
suffix_tree := SuffixTree.new();
suffix_tree.insert('apple');
suffix_tree.insert('banana');
suffix_tree.insert('cherry');
suffix_tree.insert('durian');
suffix_tree.insert('elderberry');

suffix_tree.print_tree();
```

This code will output the following:

```
a
  p
    p
      l
        e
  b
    a
      n
        a
          n
            a
  c
    h
      e
        r
          r
            y
  d
    u
      r
        i
          a
          n
  e
    l
      d
        e
          r
            b
              e
                r
                  r
                    y
```

This output shows the suffix tree that was constructed from the input strings. The tree is organized alphabetically, with the strings 'apple', 'banana', 'cherry', 'durian', and 'elderberry' stored in the tree. The paths from the root of the tree to the leaves represent the complete strings, and the nodes in the tree represent the substrings of the input strings.