```rust
struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

impl<T> Node<T> {
    fn new(value: T) -> Self {
        Node {
            value,
            next: None,
        }
    }

    fn push(&mut self, value: T) {
        match self.next {
            Some(ref mut next) => next.push(value),
            None => self.next = Some(Box::new(Node::new(value))),
        }
    }
}

struct Trie {
    root: Node<char>,
}

impl Trie {
    fn new() -> Self {
        Trie {
            root: Node::new(' '),
        }
    }

    fn insert(&mut self, word: &str) {
        let mut current = &mut self.root;
        for c in word.chars() {
            match current.next {
                Some(ref mut next) => current = next,
                None => {
                    current.next = Some(Box::new(Node::new(c)));
                    current = current.next.as_mut().unwrap();
                }
            }
        }
    }

    fn search(&self, word: &str) -> bool {
        let mut current = &self.root;
        for c in word.chars() {
            match current.next {
                Some(ref next) => {
                    if next.value == c {
                        current = next;
                    } else {
                        return false;
                    }
                }
                None => return false,
            }
        }

        current.next.is_some()
    }
}

fn main() {
    let mut trie = Trie::new();
    trie.insert("hello");
    trie.insert("world");

    assert!(trie.search("hello"));
    assert!(trie.search("world"));
    assert!(!trie.search("hell"));
    assert!(!trie.search("worl"));
}
```

This code implements a trie data structure in Rust. A trie is a tree-like data structure used to store strings in a way that allows for fast retrieval.

The Node struct represents a node in the trie. It has a value field, which stores the character stored at that node, and a next field, which points to the next node in the trie.

The Trie struct represents the entire trie. It has a root field, which points to the root node of the trie.

The insert method inserts a word into the trie. It iterates over the characters in the word and adds them to the trie one by one. If a node for a character does not exist, it is created.

The search method searches for a word in the trie. It iterates over the characters in the word and checks if there is a node for each character in the trie. If there is not, the word is not in the trie.

The main function creates a trie, inserts some words into it, and then searches for some words in it. The assert! macro is used to check that the results of the searches are correct.