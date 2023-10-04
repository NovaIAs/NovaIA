Claro! Vou gerar um código complexo em JavaScript para você. Este código é um exemplo de um algoritmo de busca em grafo usando o algoritmo de busca em largura (BFS). Ele é usado para encontrar o caminho mais curto entre dois nós em um grafo.

```javascript
class Queue {
  constructor() {
    this.items = [];
  }

  enqueue(element) {
    this.items.push(element);
  }

  dequeue() {
    if (this.isEmpty()) {
      return "Underflow";
    }
    return this.items.shift();
  }

  isEmpty() {
    return this.items.length === 0;
  }
}

class Graph {
  constructor(vertices) {
    this.vertices = vertices;
    this.adjList = new Map();
  }

  addVertex(vertex) {
    this.adjList.set(vertex, []);
  }

  addEdge(vertex1, vertex2) {
    this.adjList.get(vertex1).push(vertex2);
    this.adjList.get(vertex2).push(vertex1);
  }

  bfs(startingNode, targetNode) {
    const visited = new Set();
    const queue = new Queue();
    const path = [];

    visited.add(startingNode);
    queue.enqueue(startingNode);

    while (!queue.isEmpty()) {
      const currentNode = queue.dequeue();
      path.push(currentNode);

      if (currentNode === targetNode) {
        console.log("Caminho encontrado:", path.join(" -> "));
        return;
      }

      const neighbors = this.adjList.get(currentNode);
      for (let i = 0; i < neighbors.length; i++) {
        const neighbor = neighbors[i];
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          queue.enqueue(neighbor);
        }
      }
    }

    console.log("Caminho não encontrado!");
  }
}

// Criando o grafo
const graph = new Graph(6);

// Adicionando os vértices
const vertices = ["A", "B", "C", "D", "E", "F"];
for (let i = 0; i < vertices.length; i++) {
  graph.addVertex(vertices[i]);
}

// Adicionando as arestas
graph.addEdge("A", "B");
graph.addEdge("A", "C");
graph.addEdge("B", "D");
graph.addEdge("B", "E");
graph.addEdge("C", "E");
graph.addEdge("D", "E");
graph.addEdge("D", "F");
graph.addEdge("E", "F");

// Realizando a busca em largura
graph.bfs("A", "F");
```

Neste código, começamos definindo uma classe `Queue` que representa uma fila e possui os métodos `enqueue` para adicionar um elemento à fila, `dequeue` para remover o elemento da fila, e `isEmpty` para verificar se a fila está vazia.

Em seguida, definimos a classe `Graph` que representa o grafo. Ela possui um construtor que recebe o número de vértices do grafo, um método `addVertex` para adicionar um vértice ao grafo, e um método `addEdge` para adicionar uma aresta entre dois vértices do grafo. Além disso, a classe `Graph` possui o método `bfs` que realiza a busca em largura a partir de um nó inicial até um nó de destino.

No exemplo, criamos um grafo com 6 vértices (A, B, C, D, E, F) e adicionamos as arestas entre eles. Em seguida, chamamos o método `bfs` passando o nó inicial "A" e o nó de destino "F". O algoritmo de busca em largura irá percorrer o grafo, procurando pelo caminho mais curto entre esses dois nós.

Esse é um exemplo de código complexo em JavaScript que utiliza estruturas de dados e algoritmos para resolver um problema específico. Ele pode ser adaptado e personalizado para diferentes casos de uso. Espero que isso tenha te ajudado!