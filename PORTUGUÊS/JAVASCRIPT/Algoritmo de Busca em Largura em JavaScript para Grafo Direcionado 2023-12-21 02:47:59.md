Claro! Aqui está um exemplo de código JavaScript complexo, que possui uma implementação de um algoritmo de busca em largura em um grafo. Este código é um exemplo de aplicação prática do algoritmo, onde ele é utilizado para encontrar o caminho mais curto entre dois vértices em um grafo direcionado.

```javascript
// Definição da classe Grafo
class Grafo {
  constructor() {
    this.vertices = [];
    this.arestas = {};
  }

  // Adiciona um novo vértice ao grafo
  adicionarVertice(v) {
    this.vertices.push(v);
    this.arestas[v] = [];
  }

  // Adiciona uma nova aresta ao grafo
  adicionarAresta(v1, v2) {
    this.arestas[v1].push(v2);
  }

  // Algoritmo de busca em largura
  buscaEmLargura(inicio, fim) {
    const fila = [inicio];
    const visitados = {};
    const antecessores = {};
    let encontrado = false;

    for (const vertice of this.vertices) {
      visitados[vertice] = false;
      antecessores[vertice] = null;
    }

    visitados[inicio] = true;

    while (fila.length > 0) {
      const v = fila.shift();

      if (v === fim) {
        encontrado = true;
        break;
      }

      for (const vizinho of this.arestas[v]) {
        if (!visitados[vizinho]) {
          visitados[vizinho] = true;
          antecessores[vizinho] = v;
          fila.push(vizinho);
        }
      }
    }

    if (!encontrado) {
      return "Caminho não encontrado";
    }

    let caminho = [];
    let v = fim;

    while (v !== null) {
      caminho.unshift(v);
      v = antecessores[v];
    }

    return caminho;
  }
}

// Exemplo de utilização do algoritmo de busca em largura
const grafo = new Grafo();
grafo.adicionarVertice("A");
grafo.adicionarVertice("B");
grafo.adicionarVertice("C");
grafo.adicionarVertice("D");
grafo.adicionarVertice("E");

grafo.adicionarAresta("A", "B");
grafo.adicionarAresta("B", "C");
grafo.adicionarAresta("C", "D");
grafo.adicionarAresta("D", "E");
grafo.adicionarAresta("A", "D");

const inicio = "A";
const fim = "E";
const caminhoMaisCurto = grafo.buscaEmLargura(inicio, fim);

console.log(`O caminho mais curto entre ${inicio} e ${fim} é: ${caminhoMaisCurto.join(" -> ")}`);
```

Neste código em JavaScript, é criada uma classe `Grafo` que representa um grafo direcionado. Essa classe possui métodos para adicionar vértices e arestas ao grafo, bem como um método `buscaEmLargura` que implementa o algoritmo de busca em largura.

No exemplo de utilização do código, é criado um grafo com cinco vértices (A, B, C, D, E) e seis arestas que conectam esses vértices. Em seguida, o algoritmo de busca em largura é utilizado para encontrar o caminho mais curto entre os vértices "A" e "E". O resultado é exibido no console.

Esse código é um exemplo de implementação de um algoritmo complexo em JavaScript, que pode ser utilizado em problemas relacionados à busca de caminhos em grafos direcionados.