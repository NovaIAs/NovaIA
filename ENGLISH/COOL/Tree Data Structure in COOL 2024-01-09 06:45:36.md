```cool

class Root {
    constructor() {
        this.children = [];
    }

    addChild(child) {
        this.children.push(child);
    }

    removeChild(child) {
        const index = this.children.indexOf(child);
        if (index !== -1) {
            this.children.splice(index, 1);
        }
    }

    hasChild(child) {
        return this.children.includes(child);
    }

    traverse(callback) {
        callback(this);
        for (let i = 0; i < this.children.length; i++) {
            this.children[i].traverse(callback);
        }
    }
}

class Node extends Root {
    constructor(value) {
        super();
        this.value = value;
    }
}

const root = new Root();
const node1 = new Node(1);
const node2 = new Node(2);
const node3 = new Node(3);
const node4 = new Node(4);
const node5 = new Node(5);

root.addChild(node1);
node1.addChild(node2);
node1.addChild(node3);
node2.addChild(node4);
node3.addChild(node5);

root.traverse((node) => {
    console.log(node.value);
});

// Output:
// 1
// 2
// 4
// 3
// 5
```

This code defines a tree data structure in COOL. A tree is a hierarchical data structure in which each node can have multiple child nodes, but each node can have only one parent node. The code defines a `Root` class, which is the base class for all nodes in the tree. The `Root` class has methods for adding, removing, and checking for child nodes, as well as a `traverse` method that can be used to visit all nodes in the tree.

The `Node` class is a subclass of the `Root` class and represents a node in the tree. The `Node` class has a `value` property, which stores the value of the node.

The code then creates a root node and adds several child nodes to it. The `traverse` method is then used to visit all nodes in the tree and print their values.

The output of the code is:

```
1
2
4
3
5
```