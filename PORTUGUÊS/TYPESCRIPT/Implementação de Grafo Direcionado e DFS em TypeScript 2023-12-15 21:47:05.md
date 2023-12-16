Claro! Abaixo está um exemplo de um código complexo em TypeScript que envolve a criação de uma estrutura de dados de grafo direcionado e a implementação do algoritmo de busca em profundidade (DFS). O código é dividido em várias classes e cada uma delas tem um propósito específico. Vou explicar o código em detalhes conforme for apresentando.

```typescript
class Vertice {
  nome: string;
  visitado: boolean;

  constructor(nome: string) {
    this.nome = nome;
    this.visitado = false;
  }
}

class Aresta {
  origem: Vertice;
  destino: Vertice;

  constructor(origem: Vertice, destino: Vertice) {
    this.origem = origem;
    this.destino = destino;
  }
}

class Grafo {
  vertices: Vertice[];
  arestas: Aresta[];

  constructor() {
    this.vertices = [];
    this.arestas = [];
  }

  adicionarVertice(nome: string) {
    const vertice = new Vertice(nome);
    this.vertices.push(vertice);
  }

  adicionarAresta(origem: string, destino: string) {
    const verticeOrigem = this.vertices.find((vertice) => vertice.nome === origem);
    const verticeDestino = this.vertices.find((vertice) => vertice.nome === destino);

    if (verticeOrigem && verticeDestino) {
      const aresta = new Aresta(verticeOrigem, verticeDestino);
      this.arestas.push(aresta);
    }
  }

  dfs(verticeInicial: Vertice) {
    const pilha: Vertice[] = [];
    pilha.push(verticeInicial);

    while (pilha.length > 0) {
      const verticeAtual = pilha.pop()!;
      verticeAtual.visitado = true;
      console.log(verticeAtual.nome);

      const arestasAdjacentes = this.arestas.filter((aresta) => aresta.origem === verticeAtual);

      for (const aresta of arestasAdjacentes) {
        const verticeDestino = aresta.destino;

        if (!verticeDestino.visitado) {
          pilha.push(verticeDestino);
        }
      }
    }
  }
}

// Exemplo de uso

const grafo = new Grafo();

grafo.adicionarVertice('A');
grafo.adicionarVertice('B');
grafo.adicionarVertice('C');
grafo.adicionarVertice('D');
grafo.adicionarVertice('E');

grafo.adicionarAresta('A', 'B');
grafo.adicionarAresta('A', 'C');
grafo.adicionarAresta('B', 'D');
grafo.adicionarAresta('B', 'E');
grafo.adicionarAresta('C', 'E');

grafo.dfs(grafo.vertices[0]);
```

Explicação do código:

- A classe `Vertice` representa um vértice em um grafo e possui duas propriedades: `nome` para armazenar o nome do vértice e `visitado` para rastrear se o vértice foi visitado durante a busca em profundidade.

- A classe `Aresta` representa uma aresta em um grafo e possui duas propriedades: `origem` para armazenar o vértice de origem da aresta e `destino` para armazenar o vértice de destino da aresta.

- A classe `Grafo` representa um grafo direcionado e contém os vértices e arestas do grafo. A função `adicionarVertice` adiciona um vértice ao grafo, enquanto a função `adicionarAresta` adiciona uma aresta ao grafo, especificando os vértices de origem e destino.

- O algoritmo de busca em profundidade (DFS) é implementado na função `dfs`. Ela usa uma pilha para rastrear os vértices a serem visitados. O algoritmo começa com o vértice inicial passado como argumento e continua visitando os vértices adjacentes até que não haja mais vértices na pilha. Durante a visita, o vértice é marcado como visitado e seu nome é impresso no console.

- O exemplo de uso cria um grafo com cinco vértices (A, B, C, D, E) e adiciona as arestas correspondentes. Em seguida, chama a função `dfs` passando o primeiro vértice (A) como argumento para iniciar a busca em profundidade.

Espero que isso lhe dê uma ideia de como criar um código complexo em TypeScript. Lembre-se de que este é apenas um exemplo e existem muitas outras maneiras de implementar estruturas de dados e algoritmos em TypeScript.