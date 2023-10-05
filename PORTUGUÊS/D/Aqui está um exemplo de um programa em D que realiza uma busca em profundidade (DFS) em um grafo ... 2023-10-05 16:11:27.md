Claro! Aqui está um exemplo de um programa em D que realiza uma busca em profundidade (DFS) em um grafo não direcionado. Vou explicar cada seção do código para facilitar o entendimento:

```d
import std.stdio;
import std.container;

class Grafo
{
    private int[][] adjacencias; // Matriz de adjacências do grafo
    private int numVertices; // Número de vértices no grafo

    this(int vertices)
    {
        numVertices = vertices;
        adjacencias = new int[][](numVertices, numVertices); // Inicializa a matriz de adjacências
    }

    void adicionarAresta(int vertice1, int vertice2)
    {
        adjacencias[vertice1][vertice2] = 1; // Marca a aresta entre o vértice1 e o vértice2 como existente
        adjacencias[vertice2][vertice1] = 1; // Marca a aresta entre o vértice2 e o vértice1 como existente (grafo não direcionado)
    }

    void DFS(int verticeInicial)
    {
        bool[] visitado = new bool[](numVertices); // Array para rastrear os vértices visitados durante a busca em profundidade

        for (int i = 0; i < numVertices; i++)
        {
            visitado[i] = false; // Inicializa todos os vértices como não visitados
        }

        DFSRecursiva(verticeInicial, visitado);
    }

    private void DFSRecursiva(int vertice, bool[] visitado)
    {
        visitado[vertice] = true; // Marca o vértice atual como visitado
        writef("Visitando vértice %d\n", vertice);

        foreach (int i, valor; adjacencias[vertice])
        {
            if (valor == 1 && !visitado[i]) // Se há uma aresta entre o vértice atual e o vértice vizinho, e o vértice vizinho ainda não foi visitado
            {
                DFSRecursiva(i, visitado); // Chama a função recursiva para visitar o vértice vizinho
            }
        }
    }
}

void main()
{
    int numVertices = 7; // Número de vértices no grafo
    Grafo grafo = new Grafo(numVertices);

    grafo.adicionarAresta(0, 1);
    grafo.adicionarAresta(0, 2);
    grafo.adicionarAresta(1, 3);
    grafo.adicionarAresta(1, 4);
    grafo.adicionarAresta(2, 5);
    grafo.adicionarAresta(2, 6);

    int verticeInicial = 0; // Vértice inicial para a busca em profundidade

    writef("Executando busca em profundidade (DFS) a partir do vértice %d...\n", verticeInicial);
    grafo.DFS(verticeInicial);
}
```

Vamos começar analisando a classe `Grafo`. Ela possui dois membros de dados principais: `adjacencias`, uma matriz de adjacências que representa as arestas do grafo, e `numVertices`, que armazena o número total de vértices no grafo.

O construtor `this` é chamado quando criamos um novo objeto `Grafo` e recebe o número de vértices como argumento. Ele inicializa `numVertices` e aloca espaço para a matriz de adjacências usando a função `new`.

A função `adicionarAresta` permite adicionar uma aresta entre dois vértices. Ela marca as posições correspondentes na matriz de adjacências como 1, indicando a existência da aresta. Vale ressaltar que estamos considerando um grafo não direcionado, então também marcamos a posição inversa.

A função `DFS` é responsável por iniciar a busca em profundidade a partir de um vértice inicial. Ela cria um array `visitado` para rastrear os vértices visitados e, em seguida, chama a função `DFSRecursiva` para realizar a busca em profundidade de fato.

A função `DFSRecursiva` é uma função auxiliar que executa a busca em profundidade de forma recursiva. Ela recebe o vértice atual e o array `visitado`. Primeiro, marca o vértice atual como visitado e imprime uma mensagem informando qual vértice está sendo visitado. Em seguida, percorre todos os vértices vizinhos do vértice atual usando um loop `foreach`. Se um vértice vizinho ainda não tiver sido visitado e houver uma aresta entre o vértice atual e o vértice vizinho, a função `DFSRecursiva` é chamada recursivamente para visitar o vértice vizinho.

No bloco `main`, criamos um objeto `Grafo` com 7 vértices e adicionamos algumas arestas usando o método `adicionarAresta`. Em seguida, chamamos a função `DFS` a partir do vértice 0 para iniciar a busca em profundidade.

O programa irá imprimir uma mensagem indicando o vértice que está sendo visitado durante a busca em profundidade. Você pode modificar o número de vértices e as arestas no bloco `main` para testar com diferentes configurações de grafos.

Espero que essa explicação tenha sido útil. Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar!