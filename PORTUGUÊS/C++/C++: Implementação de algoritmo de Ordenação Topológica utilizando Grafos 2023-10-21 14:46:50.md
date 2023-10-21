Claro, vou gerar um código em C++ que implementa um algoritmo de Ordenação Topológica utilizando Grafos. Esse algoritmo é usado para ordenar vértices em um grafo direcionado acíclico (DAG), de tal forma que se a aresta (u, v) existe no grafo, então u aparece antes de v na ordenação.

O código começa com a definição de uma classe Grafo que contém um vetor de listas de adjacência, onde cada lista representa as arestas saindo de um vértice. Em seguida, é definida uma função de ajuda para fazer a ordenação topológica recursivamente. Esta função utiliza uma pilha para manter a ordem dos vértices visitados.

```cpp
#include <iostream>
#include <stack>
#include <vector>
#include <list>
using namespace std;

class Grafo {
    int V; // número de vértices
    list<int> *adj; // lista de adjacência
public:
    Grafo(int V); // construtor
    void addAresta(int v, int w); // adiciona uma aresta ao grafo
    void ordenacaoTopologica(); // realiza a ordenação topológica
};

Grafo::Grafo(int V) {
    this->V = V;
    adj = new list<int>[V];
}

void Grafo::addAresta(int v, int w) {
    adj[v].push_back(w); // adiciona uma aresta de v para w
}

void Grafo::ordenacaoTopologica() {
    // cria um vetor de visitados
    vector<bool> visitados(V, false);

    // cria uma pilha para armazenar a ordem de visitação dos vértices
    stack<int> pilha;

    // visita cada vértice recursivamente e empilha na pilha
    for (int i = 0; i < V; i++) {
        if (!visitados[i])
            ordenacaoTopologicaAux(i, visitados, pilha);
    }

    // imprime a ordenação topológica
    while (!pilha.empty()) {
        cout << pilha.top() << " ";
        pilha.pop();
    }
}

void Grafo::ordenacaoTopologicaAux(int v, vector<bool> &visitados, stack<int> &pilha) {
    // marca o vértice como visitado
    visitados[v] = true;

    // visita todos os vértices adjacentes recursivamente
    list<int>::iterator i;
    for (i = adj[v].begin(); i != adj[v].end(); ++i) {
        if (!visitados[*i])
            ordenacaoTopologicaAux(*i, visitados, pilha);
    }

    // empilha o vértice atual
    pilha.push(v);
}

int main() {
    // cria um grafo com 6 vértices
    Grafo g(6);

    // adiciona as arestas
    g.addAresta(5, 2);
    g.addAresta(5, 0);
    g.addAresta(4, 0);
    g.addAresta(4, 1);
    g.addAresta(2, 3);
    g.addAresta(3, 1);

    cout << "Ordenação topológica do grafo:\n";
    g.ordenacaoTopologica();

    return 0;
}
```

Neste código, a função `main()` cria um grafo com 6 vértices e adiciona as arestas correspondentes. Em seguida, chama a função `ordenacaoTopologica()` da classe `Grafo` para realizar a ordenação topológica dos vértices. O resultado é impresso na tela.

Este é um exemplo simples, mas o algoritmo de Ordenação Topológica é amplamente utilizado em problemas de programação e pode ser adaptado para diversas situações.