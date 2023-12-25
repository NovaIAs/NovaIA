Claro! Abaixo está um exemplo de um código complexo em TypeScript que implementa um algoritmo de busca em largura (BFS) para encontrar a menor distância entre dois pontos em um grafo:

```typescript
class Graph {
  private vertices: number;
  private adjacencyList: Map<number, number[]>;

  constructor(vertices: number) {
    this.vertices = vertices;
    this.adjacencyList = new Map<number, number[]>();
  }

  addEdge(source: number, destination: number) {
    if (!this.adjacencyList.has(source)) {
      this.adjacencyList.set(source, []);
    }
    this.adjacencyList.get(source)?.push(destination);

    if (!this.adjacencyList.has(destination)) {
      this.adjacencyList.set(destination, []);
    }
    this.adjacencyList.get(destination)?.push(source);
  }

  bfs(startVertex: number, endVertex: number): number {
    const queue: number[] = [];
    const visited: boolean[] = Array(this.vertices).fill(false);
    const distances: number[] = Array(this.vertices).fill(0);

    queue.push(startVertex);
    visited[startVertex] = true;

    while (queue.length > 0) {
      const currentVertex = queue.shift()!;
      const adjacentVertices = this.adjacencyList.get(currentVertex)!;

      for (const adjacentVertex of adjacentVertices) {
        if (!visited[adjacentVertex]) {
          queue.push(adjacentVertex);
          visited[adjacentVertex] = true;
          distances[adjacentVertex] = distances[currentVertex] + 1;

          if (adjacentVertex === endVertex) {
            return distances[adjacentVertex];
          }
        }
      }
    }

    return -1; // Caso não haja caminho entre os vértices
  }
}

const graph = new Graph(6);

graph.addEdge(0, 1);
graph.addEdge(0, 2);
graph.addEdge(1, 3);
graph.addEdge(2, 4);
graph.addEdge(4, 5);

const startVertex = 0;
const endVertex = 5;

const shortestDistance = graph.bfs(startVertex, endVertex);

console.log(`A menor distância entre o vértice ${startVertex} e o vértice ${endVertex} é ${shortestDistance}.`);
```

Neste código, a classe `Graph` representa um grafo não direcionado. O método `addEdge` é usado para adicionar arestas ao grafo. O método `bfs` implementa o algoritmo de busca em largura, que encontra a menor distância entre um vértice inicial e um vértice final.

O algoritmo de busca em largura (BFS) começa pelo vértice inicial e explora todos os vértices adjacentes antes de prosseguir para os vértices seguintes. Isso garante que a menor distância seja encontrada, já que todos os caminhos possíveis são explorados em ordem crescente de distância.

No exemplo acima, o grafo possui 6 vértices e as arestas são adicionadas através do método `addEdge`. Em seguida, é chamado o método `bfs` passando o vértice inicial e o vértice final. O resultado é exibido no console.

Espero que este exemplo te ajude a entender melhor como implementar um algoritmo de busca em largura em TypeScript!