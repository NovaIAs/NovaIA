```swift
// Define a protocol for a generic stack data structure
protocol Stack {
    associatedtype Element
    mutating func push(_ element: Element)
    mutating func pop() -> Element?
    var isEmpty: Bool { get }
    var count: Int { get }
}

// Create a concrete implementation of the stack protocol using an array
struct ArrayStack<Element>: Stack {
    private var array: [Element] = []
    
    mutating func push(_ element: Element) {
        array.append(element)
    }
    
    mutating func pop() -> Element? {
        return array.popLast()
    }
    
    var isEmpty: Bool {
        return array.isEmpty
    }
    
    var count: Int {
        return array.count
    }
}

// Create a protocol for a generic binary search tree
protocol BinarySearchTree {
    associatedtype Key: Comparable
    associatedtype Value
    func insert(_ key: Key, value: Value)
    func remove(_ key: Key)
    func search(_ key: Key) -> Value?
    var count: Int { get }
}

// Create a concrete implementation of the binary search tree protocol using a self-balancing tree
struct SelfBalancingBST<Key: Comparable, Value>: BinarySearchTree {
    private var root: Node?
    
    private struct Node {
        var key: Key
        var value: Value
        var left: Node?
        var right: Node?
        var height: Int
        
        init(key: Key, value: Value) {
            self.key = key
            self.value = value
            self.left = nil
            self.right = nil
            self.height = 1
        }
    }
    
    mutating func insert(_ key: Key, value: Value) {
        root = insert(key: key, value: value, node: root)
    }
    
    private func insert(key: Key, value: Value, node: Node?) -> Node {
        if node == nil {
            return Node(key: key, value: value)
        }
        
        if key < node!.key {
            node!.left = insert(key: key, value: value, node: node!.left)
        } else if key > node!.key {
            node!.right = insert(key: key, value: value, node: node!.right)
        } else {
            node!.value = value
        }
        
        return balance(node!)
    }
    
    mutating func remove(_ key: Key) {
        root = remove(key: key, node: root)
    }
    
    private func remove(key: Key, node: Node?) -> Node? {
        if node == nil {
            return nil
        }
        
        if key < node!.key {
            node!.left = remove(key: key, node: node!.left)
        } else if key > node!.key {
            node!.right = remove(key: key, node: node!.right)
        } else {
            if node!.left == nil {
                return node!.right
            } else if node!.right == nil {
                return node!.left
            }
            
            // Find the minimum value in the right subtree
            var minNode = node!.right
            while minNode!.left != nil {
                minNode = minNode!.left
            }
            
            // Replace the current node's key and value with the minimum value
            node!.key = minNode!.key
            node!.value = minNode!.value
            
            // Remove the minimum value from the right subtree
            node!.right = remove(key