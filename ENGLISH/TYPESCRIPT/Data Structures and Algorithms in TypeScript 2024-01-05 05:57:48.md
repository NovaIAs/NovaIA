```typescript
// Define an interface for a generic queue data structure
interface Queue<T> {
  enqueue(item: T): void;
  dequeue(): T | undefined;
  peek(): T | undefined;
  isEmpty(): boolean;
}

// Implement a queue using a linked list
class QueueLinkedList<T> implements Queue<T> {
  private head: Node<T> | null = null;
  private tail: Node<T> | null = null;
  private length: number = 0;

  enqueue(item: T): void {
    const newNode = new Node(item);
    if (this.isEmpty()) {
      this.head = newNode;
      this.tail = newNode;
    } else {
      this.tail!.next = newNode;
      this.tail = newNode;
    }
    this.length++;
  }

  dequeue(): T | undefined {
    if (this.isEmpty()) {
      return undefined;
    }
    const dequeuedItem = this.head!.data;
    this.head = this.head!.next;
    if (this.head === null) {
      this.tail = null;
    }
    this.length--;
    return dequeuedItem;
  }

  peek(): T | undefined {
    if (this.isEmpty()) {
      return undefined;
    }
    return this.head!.data;
  }

  isEmpty(): boolean {
    return this.length === 0;
  }
}

// Define an interface for a generic tree node
interface TreeNode<T> {
  data: T;
  children: TreeNode<T>[];
}

// Implement a tree using a recursive data structure
class Tree<T> {
  private root: TreeNode<T> | null = null;

  insert(data: T): void {
    if (this.root === null) {
      this.root = new TreeNode(data);
    } else {
      this._insertNode(data, this.root);
    }
  }

  private _insertNode(data: T, node: TreeNode<T>): void {
    const newNode = new TreeNode(data);
    node.children.push(newNode);
  }

  traversePreOrder(callback: (data: T) => void): void {
    this._traversePreOrder(callback, this.root);
  }

  private _traversePreOrder(callback: (data: T) => void, node: TreeNode<T> | null): void {
    if (node === null) {
      return;
    }
    callback(node.data);
    for (const child of node.children) {
      this._traversePreOrder(callback, child);
    }
  }

  traversePostOrder(callback: (data: T) => void): void {
    this._traversePostOrder(callback, this.root);
  }

  private _traversePostOrder(callback: (data: T) => void, node: TreeNode<T> | null): void {
    if (node === null) {
      return;
    }
    for (const child of node.children) {
      this._traversePostOrder(callback, child);
    }
    callback(node.data);
  }
}

// Define an interface for a generic graph node
interface GraphNode<T> {
  data: T;
  neighbors: GraphNode<T>[];
}

// Implement a graph using an adjacency list
class Graph<T> {
  private nodes: GraphNode<T>[] = [];

  addNode(data: T): GraphNode<T> {
    const newNode = new GraphNode(data);
    this.nodes.push(newNode);
    return newNode;
  }

  addEdge(node1: GraphNode<T>, node2: GraphNode<T>): void {
    node1.neighbors.push(node2);
    node2.neighbors.push(node1);
  }

  traverseBreadthFirst(startNode: GraphNode<T>, callback: (data: T) => void): void {
    const visitedNodes: GraphNode<T>[] = [];
    const queue = new QueueLinkedList<GraphNode<T>>();
    queue.enqueue(startNode);

    while (!queue.isEmpty()) {
      const currentNode = queue.dequeue();
      if (currentNode === undefined) {
        continue;
      }
      if (visitedNodes.includes(currentNode)) {
        continue;
      }
      visitedNodes.push(currentNode);
      callback(currentNode.data);

      for (const neighbor of currentNode.neighbors) {
        if (!visitedNodes.includes(neighbor)) {
          queue.enqueue(neighbor);
        }
      }
    }
  }

  traverseDepthFirst(startNode: GraphNode<T>, callback: (data: T) => void): void {
    const visitedNodes: GraphNode<T>[] = [];
    const stack = [];
    stack.push(startNode);

    while (stack.length > 0) {
      const currentNode = stack.pop();
      if (currentNode === undefined) {
        continue;
      }
      if (visitedNodes.includes(currentNode)) {
        continue;
      }
      visitedNodes.push(currentNode);
      callback(currentNode.data);

      for (const neighbor of currentNode.neighbors) {
        if (!visitedNodes.includes(neighbor)) {
          stack.push(neighbor);
        }
      }
    }
  }
}

// Example usage
const myQueue = new QueueLinkedList<number>();
myQueue.enqueue(1);
myQueue.enqueue(2);
myQueue.enqueue(3);
console.log(myQueue.dequeue()); // 1
console.log(myQueue.peek()); // 2

const myTree = new Tree<string>();
myTree.insert("A");
myTree.insert("B");
myTree.insert("C");
myTree.insert("D");
myTree.insert("E");
myTree.insert("F");
myTree.insert("G");
myTree.insert("H");

console.log("Pre-order traversal:");
myTree.traversePreOrder((data) => {
  console.log(data);
});

console.log("Post-order traversal:");
myTree.traversePostOrder((data) => {
  console.log(data);
});

const myGraph = new Graph<string>();
const nodeA = myGraph.addNode("A");
const nodeB = myGraph.addNode("B");
const nodeC = myGraph.addNode("C");
const nodeD = myGraph.addNode("D");
const nodeE = myGraph.addNode("E");
const nodeF = myGraph.addNode("F");
const nodeG = myGraph.addNode("G");
const nodeH = myGraph.addNode("H");

myGraph.addEdge(nodeA, nodeB);
myGraph.addEdge(nodeA, nodeC);
myGraph.addEdge(nodeB, nodeD);
myGraph.addEdge(nodeB, nodeE);
myGraph.addEdge(nodeC, nodeF);
myGraph.addEdge(nodeC, nodeG);
myGraph.addEdge(nodeD, nodeH);
myGraph.addEdge(nodeE, nodeH);

console.log("Breadth-first traversal:");
myGraph.traverseBreadthFirst(nodeA, (data) => {
  console.log(data);
});

console.log("Depth-first traversal:");
myGraph.traverseDepthFirst(nodeA, (data) => {
  console.log(data);
});
```

Explanation:

1. QueueLinkedList: This class implements a queue data structure using a linked list. It has methods to enqueue, dequeue, peek, and check if the queue is empty.

2. Tree: This class implements a tree data structure using a recursive approach. It has methods to insert nodes, traverse the tree in pre-order and post-order, and print the data of each node.

3. Graph: This class implements a graph data structure using an adjacency list. It has methods to add nodes, add edges, and traverse the graph using breadth-first and depth-first search algorithms.

4. Example Usage:

   - Queue: We create a queue, enqueue some items, and then dequeue and peek at them.

   - Tree: We create a tree, insert some nodes, and then traverse it in pre-order and post-order.

   - Graph: We create a graph, add nodes and edges, and then traverse it using breadth-first and depth-first search.

This code demonstrates the implementation of various data structures and algorithms in TypeScript. It includes a queue using a linked list, a tree using recursion, and a graph using an adjacency list. It also shows how to use these data structures and algorithms to perform different operations, such as enqueueing and dequeueing items from a queue, inserting and traversing nodes in a tree, and traversing a graph using breadth-first and depth-first search.