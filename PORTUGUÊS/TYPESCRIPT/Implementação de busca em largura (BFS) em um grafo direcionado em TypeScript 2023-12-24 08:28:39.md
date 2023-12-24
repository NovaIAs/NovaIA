Claro! Aqui está um código TypeScript complexo que envolve uma implementação de um algoritmo de busca em largura (BFS) em um grafo direcionado. O código está em português e contém comentários explicativos para facilitar a compreensão:

```typescript
// Definição da classe para representar um nó no grafo
class No {
  valor: string;
  vizinhos: No[];

  constructor(valor: string) {
    this.valor = valor;
    this.vizinhos = [];
  }
}

// Definição da classe para representar o grafo
class Grafo {
  nos: No[];

  constructor() {
    this.nos = [];
  }

  // Método para adicionar um nó ao grafo
  adicionarNo(valor: string): void {
    const novoNo = new No(valor);
    this.nos.push(novoNo);
  }

  // Método para adicionar uma aresta direcionada entre dois nós
  adicionarAresta(origem: string, destino: string): void {
    const noOrigem = this.nos.find((no) => no.valor === origem);
    const noDestino = this.nos.find((no) => no.valor === destino);

    if (noOrigem && noDestino) {
      noOrigem.vizinhos.push(noDestino);
    }
  }

  // Método para realizar a busca em largura (BFS) no grafo
  bfs(origem: string): string[] {
    const visitados: Set<No> = new Set();
    const fila: No[] = [];
    const resultado: string[] = [];

    const noOrigem = this.nos.find((no) => no.valor === origem);

    if (noOrigem) {
      fila.push(noOrigem);
      visitados.add(noOrigem);

      while (fila.length > 0) {
        const noAtual = fila.shift();
        resultado.push(noAtual.valor);

        noAtual.vizinhos.forEach((vizinho) => {
          if (!visitados.has(vizinho)) {
            visitados.add(vizinho);
            fila.push(vizinho);
          }
        });
      }
    }

    return resultado;
  }
}

// Exemplo de uso do grafo e busca em largura
const meuGrafo = new Grafo();

// Adicionando nós ao grafo
meuGrafo.adicionarNo("A");
meuGrafo.adicionarNo("B");
meuGrafo.adicionarNo("C");
meuGrafo.adicionarNo("D");
meuGrafo.adicionarNo("E");

// Adicionando arestas entre os nós
meuGrafo.adicionarAresta("A", "B");
meuGrafo.adicionarAresta("A", "C");
meuGrafo.adicionarAresta("B", "D");
meuGrafo.adicionarAresta("C", "D");
meuGrafo.adicionarAresta("D", "E");

// Realizando a busca em largura a partir do nó "A"
const resultadoBFS = meuGrafo.bfs("A");

// Exibindo o resultado
console.log("Resultado BFS:", resultadoBFS);
```

Neste código, estamos criando um grafo direcionado com cinco nós (A, B, C, D e E) e definindo as relações entre eles através das arestas. Em seguida, realizamos uma busca em largura (BFS) a partir do nó "A" e exibimos o resultado no console.

A busca em largura é um algoritmo utilizado para percorrer ou buscar elementos em uma estrutura de dados do tipo grafo. Nesse algoritmo, visitamos todos os nós vizinhos de um nó antes de visitar os vizinhos dos vizinhos. Isso é feito utilizando uma fila para controlar a ordem de visita dos nós.

Espero que este código complexo em TypeScript tenha atendido às suas expectativas!