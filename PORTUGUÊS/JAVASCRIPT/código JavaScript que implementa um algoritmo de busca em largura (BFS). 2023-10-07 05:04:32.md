Claro! Aqui está um código JavaScript que implementa um algoritmo de busca em largura (BFS) em um grafo direcionado. O algoritmo percorre o grafo começando a partir de um nó inicial e visita todos os nós alcançáveis a partir dele.

```javascript
// Definindo a classe do Grafo
class Grafo {
  constructor() {
    this.vertices = []; // Lista de vértices do grafo
    this.adjacentes = new Map(); // Armazenar os vértices adjacentes de cada vértice
  }

  // Adicionando um novo vértice ao grafo
  adicionarVertice(v) {
    this.vertices.push(v);
    this.adjacentes.set(v, []);
  }

  // Adicionando uma aresta direcionada entre dois vértices
  adicionarAresta(v1, v2) {
    this.adjacentes.get(v1).push(v2);
  }

  // Implementando o algoritmo BFS
  bfs(inicial) {
    // Inicializando os arrays de visitados e fila
    const visitados = new Array(this.vertices.length).fill(false);
    const fila = [];

    // Marcando o nó inicial como visitado e inserindo-o na fila
    visitados[inicial] = true;
    fila.push(inicial);

    // Enquanto a fila não estiver vazia
    while (fila.length > 0) {
      // Removendo o primeiro elemento da fila e imprimindo-o
      const verticeAtual = fila.shift();
      console.log(verticeAtual);

      // Obtendo todos os vértices adjacentes ao vértice atual
      const adjacentes = this.adjacentes.get(verticeAtual);

      // Percorrendo todos os vértices adjacentes
      for (let i = 0; i < adjacentes.length; i++) {
        const adjacente = adjacentes[i];

        // Se o vértice adjacente ainda não foi visitado
        if (!visitados[adjacente]) {
          // Marcando o vértice adjacente como visitado e inserindo-o na fila
          visitados[adjacente] = true;
          fila.push(adjacente);
        }
      }
    }
  }
}

// Exemplo de utilização do grafo e do algoritmo BFS
const grafo = new Grafo();
grafo.adicionarVertice(0);
grafo.adicionarVertice(1);
grafo.adicionarVertice(2);
grafo.adicionarVertice(3);
grafo.adicionarVertice(4);
grafo.adicionarVertice(5);

grafo.adicionarAresta(0, 1);
grafo.adicionarAresta(0, 2);
grafo.adicionarAresta(1, 3);
grafo.adicionarAresta(2, 4);
grafo.adicionarAresta(3, 4);
grafo.adicionarAresta(3, 5);
grafo.adicionarAresta(4, 5);

console.log("Ordem de visita dos nós:");
grafo.bfs(0);
```

Neste código, primeiro definimos a classe `Grafo`, que possui um array `vertices` para armazenar os vértices do grafo e um `Map` chamado `adjacentes` para armazenar os vértices adjacentes de cada vértice. Em seguida, implementamos os métodos `adicionarVertice` e `adicionarAresta` para adicionar vértices e arestas direcionadas ao grafo.

A parte principal do código é o método `bfs`, que implementa o algoritmo de busca em largura (BFS). O algoritmo começa iniciando um array de visitados e uma fila. O nó inicial é marcado como visitado e inserido na fila. Enquanto a fila não estiver vazia, removemos o primeiro elemento da fila, imprimimos seu valor e percorremos todos os vértices adjacentes a ele. Se um vértice adjacente ainda não foi visitado, ele é marcado como visitado e inserido na fila.

No exemplo de utilização do grafo e do algoritmo BFS, criamos um grafo com 6 vértices e 7 arestas. Aplicamos o algoritmo BFS a partir do vértice 0 e imprimimos a ordem de visita dos nós.

Espero que este código complexo seja útil para você! Se você precisar de mais ajuda ou tiver alguma dúvida, estou à disposição para ajudar.
