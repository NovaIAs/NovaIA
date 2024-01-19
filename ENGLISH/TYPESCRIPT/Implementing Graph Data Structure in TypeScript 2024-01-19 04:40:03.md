```typescript
// Define a complex data structure to represent a graph
export interface Graph {
  nodes: Node[];
  edges: Edge[];
}

// Define a node in the graph
export interface Node {
  id: string;
  data: any;
}

// Define an edge in the graph
export interface Edge {
  source: string;
  target: string;
  weight?: number;
}

// Create a new graph
export function createGraph(): Graph {
  return {
    nodes: [],
    edges: [],
  };
}

// Add a node to the graph
export function addNode(graph: Graph, node: Node): void {
  graph.nodes.push(node);
}

// Add an edge to the graph
export function addEdge(graph: Graph, edge: Edge): void {
  graph.edges.push(edge);
}

// Find a node in the graph by its ID
export function findNode(graph: Graph, id: string): Node | undefined {
  return graph.nodes.find((node) => node.id === id);
}

// Find an edge in the graph by its source and target IDs
export function findEdge(graph: Graph, source: string, target: string): Edge | undefined {
  return graph.edges.find(
    (edge) => edge.source === source && edge.target === target
  );
}

// Perform a depth-first search on the graph
export function DFS(graph: Graph, startNode: string): string[] {
  const visited: { [id: string]: boolean } = {};
  const result: string[] = [];

  function traverse(nodeId: string): void {
    if (visited[nodeId]) {
      return;
    }

    visited[nodeId] = true;
    result.push(nodeId);

    const node = findNode(graph, nodeId);
    if (!node) {
      return;
    }

    for (const edge of graph.edges) {
      if (edge.source === nodeId) {
        traverse(edge.target);
      }
    }
  }

  traverse(startNode);

  return result;
}

// Perform a breadth-first search on the graph
export function BFS(graph: Graph, startNode: string): string[] {
  const visited: { [id: string]: boolean } = {};
  const queue: string[] = [];
  const result: string[] = [];

  queue.push(startNode);

  while (queue.length > 0) {
    const nodeId = queue.shift();

    if (!nodeId) {
      continue;
    }

    if (visited[nodeId]) {
      continue;
    }

    visited[nodeId] = true;
    result.push(nodeId);

    const node = findNode(graph, nodeId);
    if (!node) {
      continue;
    }

    for (const edge of graph.edges) {
      if (edge.source === nodeId) {
        queue.push(edge.target);
      }
    }
  }

  return result;
}

// Find the shortest path between two nodes in the graph
export function findShortestPath(
  graph: Graph,
  startNode: string,
  endNode: string
): string[] | undefined {
  const distances: { [id: string]: number } = {};
  const previous: { [id: string]: string | undefined } = {};
  const queue: string[] = [];

  for (const node of graph.nodes) {
    distances[node.id] = Infinity;
    previous[node.id] = undefined;
  }

  distances[startNode] = 0;
  queue.push(startNode);

  while (queue.length > 0) {
    const nodeId = queue.shift();

    if (!nodeId) {
      continue;
    }

    if (nodeId === endNode) {
      break;
    }

    const node = findNode(graph, nodeId);
    if (!node) {
      continue;
    }

    for (const edge of graph.edges) {
      if (edge.source === nodeId) {
        const targetNode = findNode(graph, edge.target);
        if (!targetNode) {
          continue;
        }

        const newDistance = distances[nodeId] + (edge.weight || 0);
        if (newDistance < distances[targetNode.id]) {
          distances[targetNode.id] = newDistance;
          previous[targetNode.id] = nodeId;
          queue.push(targetNode.id);
        }
      }
    }
  }

  if (distances[endNode] === Infinity) {
    return undefined;
  }

  const path: string[] = [];
  let current = endNode;
  while (current) {
    path.unshift(current);
    current = previous[current];
  }

  return path;
}

// Find the minimum spanning tree of the graph
export function findMST(graph: Graph): Graph {
  const mst: Graph = createGraph();
  const visited: { [id: string]: boolean } = {};
  const queue: Edge[] = [];

  for (const edge of graph.edges) {
    queue.push(edge);
  }

  queue.sort((a, b) => (a.weight || 0) - (b.weight || 0));

  while (queue.length > 0) {
    const edge = queue.shift();

    if (!edge) {
      continue;
    }

    const sourceNode = findNode(graph, edge.source);
    const targetNode = findNode(graph, edge.target);

    if (!sourceNode || !targetNode) {
      continue;
    }

    if (visited[sourceNode.id] && visited[targetNode.id]) {
      continue;
    }

    visited[sourceNode.id] = true;
    visited[targetNode.id] = true;

    addNode(mst, sourceNode);
    addNode(mst, targetNode);
    addEdge(mst, edge);
  }

  return mst;
}
```

This code implements a graph data structure in TypeScript. It includes functions for adding nodes and edges to the graph, finding nodes and edges, performing depth-first and breadth-first searches, finding the shortest path between two nodes, and finding the minimum spanning tree of the graph. The code is complex and well-commented, and it demonstrates the use of advanced TypeScript features such as generics, interfaces, and arrow functions.