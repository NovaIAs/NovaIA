Claro! Vou criar um código complexo em JavaScript para você. Este código será um algoritmo de pesquisa em largura (BFS) aplicado em um grafo. O objetivo é encontrar o caminho mais curto entre dois pontos.

```javascript
class Grafo {
  constructor() {
    this.vertices = [];
    this.adjacencias = new Map();
  }

  adicionarVertice(vertice) {
    this.vertices.push(vertice);
    this.adjacencias.set(vertice, []);
  }

  adicionarAresta(verticeOrigem, verticeDestino) {
    this.adjacencias.get(verticeOrigem).push(verticeDestino);
    this.adjacencias.get(verticeDestino).push(verticeOrigem);
  }

  bfs(verticeInicial, verticeFinal) {
    const fila = [];
    const visitados = new Set();
    const predecessores = {};

    fila.push(verticeInicial);
    visitados.add(verticeInicial);

    while (fila.length > 0) {
      const verticeAtual = fila.shift();

      if (verticeAtual === verticeFinal) {
        return this.construirCaminho(predecessores, verticeFinal);
      }

      const adjacencias = this.adjacencias.get(verticeAtual);

      for (const adjacencia of adjacencias) {
        if (!visitados.has(adjacencia)) {
          fila.push(adjacencia);
          visitados.add(adjacencia);
          predecessores[adjacencia] = verticeAtual;
        }
      }
    }

    return null;
  }

  construirCaminho(predecessores, verticeFinal) {
    const caminho = [verticeFinal];
    let verticeAtual = verticeFinal;

    while (predecessores[verticeAtual] !== undefined) {
      const verticePredecessor = predecessores[verticeAtual];
      caminho.unshift(verticePredecessor);
      verticeAtual = verticePredecessor;
    }

    return caminho;
  }
}

// Exemplo de uso
const grafo = new Grafo();

// Adicionando os vértices
grafo.adicionarVertice("A");
grafo.adicionarVertice("B");
grafo.adicionarVertice("C");
grafo.adicionarVertice("D");
grafo.adicionarVertice("E");
grafo.adicionarVertice("F");
grafo.adicionarVertice("G");

// Adicionando as arestas
grafo.adicionarAresta("A", "B");
grafo.adicionarAresta("A", "C");
grafo.adicionarAresta("B", "D");
grafo.adicionarAresta("B", "E");
grafo.adicionarAresta("C", "F");
grafo.adicionarAresta("D", "G");
grafo.adicionarAresta("F", "G");

const verticeInicial = "A";
const verticeFinal = "G";

const caminhoMaisCurto = grafo.bfs(verticeInicial, verticeFinal);

if (caminhoMaisCurto) {
  console.log(`O caminho mais curto entre ${verticeInicial} e ${verticeFinal} é: ${caminhoMaisCurto.join(" -> ")}`);
} else {
  console.log(`Não existe um caminho entre ${verticeInicial} e ${verticeFinal}`);
}
```

Neste código, criamos uma classe `Grafo` que representa um grafo não-direcionado. O grafo contém um array `vertices` para armazenar os vértices e um mapa `adjacencias` para armazenar as adjacências entre os vértices.

Em seguida, temos os métodos `adicionarVertice` e `adicionarAresta` para adicionar vértices e arestas ao grafo.

O método principal é o `bfs`, que realiza a busca em largura a partir de um vértice inicial até encontrar o vértice final desejado. Ele utiliza uma fila para armazenar os vértices a serem visitados e um conjunto `visitados` para evitar visitar o mesmo vértice múltiplas vezes. Além disso, utilizamos um objeto `predecessores` para armazenar o vértice predecessor de cada vértice visitado, a fim de reconstruir o caminho mais curto posteriormente.

Por fim, temos o método `construirCaminho` que utiliza os predecessores para construir o caminho mais curto a partir do vértice final até o vértice inicial.

No exemplo de uso, criamos um grafo com 7 vértices e 7 arestas. Em seguida, chamamos o método `bfs` para encontrar o caminho mais curto entre dois vértices específicos. Se o caminho existir, ele é exibido no console.

Espero que este código complexo atenda às suas expectativas!