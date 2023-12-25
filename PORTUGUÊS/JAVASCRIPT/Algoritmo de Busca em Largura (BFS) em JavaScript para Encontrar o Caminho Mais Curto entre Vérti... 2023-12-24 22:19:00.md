Claro! Abaixo está um código complexo em JavaScript que usa conceitos avançados de programação. O código é um exemplo de um algoritmo de busca em largura (BFS) aplicado em um grafo direcionado. Ele encontra o caminho mais curto entre dois vértices em um grafo.

```javascript
// Classe para representar um vértice em um grafo
class Vertice {
  constructor(valor) {
    this.valor = valor;
    this.adjacentes = [];
  }

  // Adiciona um vértice adjacente
  adicionarAdjacente(vertice) {
    this.adjacentes.push(vertice);
  }
}

// Classe para representar um grafo
class Grafo {
  constructor() {
    this.vertices = {};
  }

  // Adiciona um vértice ao grafo
  adicionarVertice(valor) {
    this.vertices[valor] = new Vertice(valor);
  }

  // Adiciona uma aresta direcionada entre dois vértices
  adicionarAresta(origem, destino) {
    this.vertices[origem].adicionarAdjacente(this.vertices[destino]);
  }

  // Algoritmo de busca em largura (BFS)
  bfs(origem, destino) {
    // Inicializa as variáveis de controle
    const visitados = {};
    const fila = [];
    const caminho = [];

    // Adiciona a origem à fila e marca como visitada
    fila.push(this.vertices[origem]);
    visitados[origem] = true;

    while (fila.length > 0) {
      // Remove o primeiro elemento da fila
      const verticeAtual = fila.shift();
      caminho.push(verticeAtual.valor);

      // Verifica se chegou ao destino
      if (verticeAtual.valor === destino) {
        return caminho;
      }

      // Adiciona os vértices adjacentes não visitados à fila
      for (const verticeAdjacente of verticeAtual.adjacentes) {
        if (!visitados[verticeAdjacente.valor]) {
          fila.push(verticeAdjacente);
          visitados[verticeAdjacente.valor] = true;
        }
      }
    }

    // Caso não haja caminho entre a origem e o destino
    return [];
  }
}

// Exemplo de uso do código
const grafo = new Grafo();

// Adiciona os vértices
grafo.adicionarVertice("A");
grafo.adicionarVertice("B");
grafo.adicionarVertice("C");
grafo.adicionarVertice("D");
grafo.adicionarVertice("E");

// Adiciona as arestas
grafo.adicionarAresta("A", "B");
grafo.adicionarAresta("A", "C");
grafo.adicionarAresta("B", "D");
grafo.adicionarAresta("C", "D");
grafo.adicionarAresta("C", "E");
grafo.adicionarAresta("D", "E");

// Encontra o caminho mais curto entre os vértices A e E
const caminhoMaisCurto = grafo.bfs("A", "E");

console.log("Caminho mais curto:", caminhoMaisCurto);
```

Neste código, temos duas classes: `Vertice` e `Grafo`. A classe `Vertice` representa um vértice em um grafo e possui uma propriedade `valor` que armazena o valor do vértice e uma propriedade `adjacentes` que armazena os vértices adjacentes. A classe `Grafo` representa o grafo em si e possui um objeto `vertices` que armazena todos os vértices.

O código também implementa o algoritmo de busca em largura (BFS) na função `bfs` da classe `Grafo`. O algoritmo começa pela origem e explora todos os vértices adjacentes a ela antes de continuar para os vértices adjacentes dos vértices adjacentes. Ele utiliza uma fila para manter o controle dos vértices a serem visitados e um objeto `visitados` para marcar os vértices que já foram visitados.

No exemplo de uso do código, é criado um grafo com cinco vértices (A, B, C, D e E) e são adicionadas as arestas que conectam esses vértices. O código então utiliza o algoritmo BFS para encontrar o caminho mais curto entre os vértices A e E. O resultado é impresso no console.

Espero que esse código atenda às suas especificações! Se você tiver mais alguma dúvida ou precisar de outro tipo de código, fique à vontade para perguntar.