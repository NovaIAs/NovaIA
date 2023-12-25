Claro! Aqui está um código complexo em TypeScript que implementa um algoritmo de busca em largura (BFS) em um grafo direcionado:

```typescript
class Grafo {
    private vertices: number;
    private adjacencias: number[][];

    constructor(vertices: number) {
        this.vertices = vertices;
        this.adjacencias = new Array(vertices);

        for (let i = 0; i < vertices; ++i) {
            this.adjacencias[i] = [];
        }
    }

    adicionarAresta(origem: number, destino: number): void {
        this.adjacencias[origem].push(destino);
    }

    buscaEmLargura(verticeInicial: number): void {
        const visitados: boolean[] = new Array(this.vertices).fill(false);
        const fila: number[] = [];

        visitados[verticeInicial] = true;
        fila.push(verticeInicial);

        while (fila.length !== 0) {
            const verticeAtual = fila.shift();

            console.log(`Visitando o vértice ${verticeAtual}`);

            for (const adjacente of this.adjacencias[verticeAtual]) {
                if (!visitados[adjacente]) {
                    visitados[adjacente] = true;
                    fila.push(adjacente);
                }
            }
        }
    }
}

// Exemplo de uso:
const grafo = new Grafo(6);
grafo.adicionarAresta(0, 1);
grafo.adicionarAresta(0, 2);
grafo.adicionarAresta(1, 3);
grafo.adicionarAresta(1, 4);
grafo.adicionarAresta(2, 4);
grafo.adicionarAresta(3, 4);
grafo.adicionarAresta(3, 5);
grafo.adicionarAresta(4, 5);

console.log("Busca em Largura a partir do vértice 0:");
grafo.buscaEmLargura(0);
```

Neste código, a classe `Grafo` representa um grafo direcionado com base em uma matriz de adjacências. O construtor da classe cria a matriz de adjacências com base no número de vértices fornecido.

O método `adicionarAresta` permite adicionar uma aresta entre dois vértices do grafo. Ele simplesmente insere o vértice de destino no array de adjacências do vértice de origem.

O método `buscaEmLargura` implementa o algoritmo BFS. Ele recebe o índice do vértice inicial como parâmetro. A variável `visitados` é um array booleano que mantém o controle de quais vértices já foram visitados. A fila `fila` é usada para armazenar os vértices a serem visitados.

O algoritmo começa marcando o vértice inicial como visitado e o adiciona à fila. Em seguida, entra em um loop que continua enquanto a fila não estiver vazia. A cada iteração, o vértice atual é removido da fila e visitado. Em seguida, percorremos todos os vértices adjacentes ao vértice atual e, se algum deles ainda não foi visitado, o marcamos como visitado e o adicionamos à fila.

No exemplo de uso fornecido, o grafo possui 6 vértices e algumas arestas foram adicionadas. Em seguida, é realizada uma busca em largura a partir do vértice 0, e os vértices visitados são exibidos no console.